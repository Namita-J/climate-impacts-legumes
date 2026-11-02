"""Build the crop yield explorer page (Yield-projections_explorer.html).

Usage: python explorer/build_explorer.py <xlsx> <africa.geojson> <data.json> [template.html out.html]
The Africa outline comes from explorer/africa_outline.R (rnaturalearth).

Replicates the bootstrapping.R approach: dp ~ poly(dt, 2) per adaptation group,
3x IQR outlier removal per crop x group, 1000 study-stratified bootstrap draws,
95% percentile bands.
"""
import json
import sys

import numpy as np
import pandas as pd

XLS, GEO, OUT = sys.argv[1], sys.argv[2], sys.argv[3]
d = pd.read_excel(XLS)

CROP_FIX = {"Pearl Millet": "Pearl millet", "Sugercane": "Sugarcane", "Pinapple": "Pineapple",
            "Bush Bean": "Common Bean", "Climbing Bean": "Common Bean"}
LEGUMES = {"Bambara", "Chickpea", "Common Bean", "Cowpea", "Faba Bean", "Green grams", "Groundnut", "Soybean"}
COUNTRY_FIX = {"Eygpt": "Egypt", "Ivory Coast": "Côte d'Ivoire", "Cote D'lvoire": "Côte d'Ivoire", "Sorghum": None}
MULTI = {"West Africa", "Central Sahel", "West Sahel", "East Africa", "Southern Africa", "Sudan Savanna",
         "Western Sahel", "Sub-Saharan Africa", "Africa", "Eastern Africa", "Sahel"}


def scen(s):
    s = str(s)
    if s in ("RCP2.6", "SSP126", "B1"):
        return "Low"
    if s in ("RCP4.5", "RCP6.0", "B2", "A1B", "SSP245"):
        return "Medium"
    if s in ("RCP8.5", "SSP585", "SSP370", "A2", "A2b", "A1FI"):
        return "High"
    return "Mixed / other"


def horizon(y):
    if pd.isna(y):
        return None
    return "Near (≤2040)" if y <= 2040 else ("Mid (2041–70)" if y <= 2070 else "End (>2070)")


def adapt(a):
    a = str(a)
    if a == "No":
        return "No adaptation"
    if a.startswith("Combined"):
        return "Bundled"
    if a.startswith("Cultivar"):
        return "Variety"
    if "ertili" in a:
        return "Fertilizer"
    if a == "Irrigation":
        return "Irrigation"
    if "lanting" in a:
        return "Planting date"
    if a in ("Mulch", "Others"):
        return "Other"
    return None  # Yield Potential, NaN


d["crop"] = d["Crop"].replace(CROP_FIX)
d = d[d["crop"].notna()].copy()
d["country"] = d["Country"].replace(COUNTRY_FIX)
d["scen"] = d["Climate scenario"].map(scen)
d["hor"] = d["Future_Mid-point"].map(horizon)
d["adapt"] = d["Adaptation type"].map(adapt)
d["co2"] = np.where(d["CO2"] == "Yes", "with", "without")
d["dp"] = pd.to_numeric(d["Climate impacts (%)"], errors="coerce")
d["dt"] = pd.to_numeric(d["temp_change_nextgddp"], errors="coerce")
d["study"] = d["Ref No"]
d["model"] = d["Crop Model"].fillna("Not recorded").str.replace(r"\s*\(Baseline available\)", "", regex=True)

# ---------- discovery ----------
crops = []
for c, g in d.groupby("crop"):
    na = g[(g.adapt == "No adaptation") & g.dp.notna()]
    crops.append({
        "crop": c, "legume": c in LEGUMES, "rows": len(g), "studies": int(g.study.nunique()),
        "countries": int(g.country.dropna().loc[lambda s: ~s.isin(MULTI)].nunique()),
        "with_dt": int(g.dt.notna().sum()), "co2_yes": int((g.co2 == "with").sum()),
        "adapt_rows": int(g.adapt.isin(["Bundled", "Variety", "Fertilizer", "Irrigation", "Planting date", "Other"]).sum()),
        "baseline_t": int(g["Baseline yield (t/ha)"].notna().sum()),
        "median_noad": None if na.empty else round(float(na.dp.median()), 1),
        "years": [int(g["Publication year"].min()), int(g["Publication year"].max())] if g["Publication year"].notna().any() else None,
    })
crops.sort(key=lambda r: -r["rows"])


def xtab(col, order):
    out = {}
    for c, g in d.groupby("crop"):
        out[c] = {k: [int((g[col] == k).sum()), int(g.loc[g[col] == k, "study"].nunique())] for k in order}
    return out


SCEN = ["Low", "Medium", "High", "Mixed / other"]
HOR = ["Near (≤2040)", "Mid (2041–70)", "End (>2070)"]
AD = ["No adaptation", "Variety", "Fertilizer", "Irrigation", "Planting date", "Bundled", "Other"]

sites = (d.dropna(subset=["latitude", "longitude"])
         .assign(lat=lambda x: x.latitude.round(2), lon=lambda x: x.longitude.round(2))
         .groupby(["lat", "lon"])
         .agg(rows=("ID", "size"), studies=("study", "nunique"),
              crops=("crop", lambda s: sorted(s.unique().tolist())),
              country=("country", lambda s: s.dropna().iloc[0] if s.notna().any() else ""),
              scale=("Scale", lambda s: s.mode().iloc[0] if s.notna().any() else ""))
         .reset_index())
sites = [{"lat": r.lat, "lon": r.lon, "n": int(r.rows), "s": int(r.studies), "c": r.crops, "k": r.country,
          "sc": r.scale} for r in sites.itertuples()]

country = (d[d.country.notna()].groupby("country")
           .agg(rows=("ID", "size"), studies=("study", "nunique")).reset_index()
           .sort_values("rows", ascending=False))
country = [{"country": r.country, "rows": int(r.rows), "studies": int(r.studies), "multi": r.country in MULTI}
           for r in country.itertuples()]

models = d.groupby("model").agg(rows=("ID", "size"), studies=("study", "nunique")).reset_index()
models = models.sort_values("rows", ascending=False)
models = [{"model": r.model, "rows": int(r.rows), "studies": int(r.studies)} for r in models.itertuples()]

src = d.groupby("Methods").agg(rows=("ID", "size"), studies=("study", "nunique")).reset_index()
src = [{"src": r.Methods, "rows": int(r.rows), "studies": int(r.studies)} for r in src.itertuples()]

# ---------- bootstrap fits ----------
rng = np.random.default_rng(123)
R = 1000
GROUPS = ["No adaptation", "Variety", "Fertilizer", "Irrigation", "Planting date", "Bundled"]


def iqr_filter(g):
    q1, q3 = g.dp.quantile([0.25, 0.75])
    i = q3 - q1
    return g[(g.dp >= q1 - 3 * i) & (g.dp <= q3 + 3 * i)]


def boot_fit(g):
    x, y, s = g.dt.to_numpy(), g.dp.to_numpy(), g.study.to_numpy()
    idx_by = [np.flatnonzero(s == k) for k in np.unique(s)]
    grid = np.linspace(x.min(), x.max(), 60)
    G = np.vstack([np.ones_like(grid), grid, grid ** 2]).T
    preds = np.empty((R, grid.size))
    for b in range(R):
        idx = np.concatenate([rng.choice(ix, ix.size, replace=True) for ix in idx_by])
        X = np.vstack([np.ones(idx.size), x[idx], x[idx] ** 2]).T
        beta, *_ = np.linalg.lstsq(X, y[idx], rcond=None)
        preds[b] = G @ beta
    return grid, preds


def at(grid, preds, t):
    if t < grid[0] or t > grid[-1]:
        return None
    p = np.array([np.interp(t, grid, row) for row in preds])
    return [round(float(p.mean()), 1), round(float(np.percentile(p, 2.5)), 1), round(float(np.percentile(p, 97.5)), 1)]


fits = {}
FIT_CROPS = [c["crop"] for c in crops if c["with_dt"] >= 40]
for c in FIT_CROPS:
    fits[c] = {}
    for co2 in ("all",):
        base = d[(d.crop == c) & d.dp.notna() & d.dt.notna() & d.adapt.isin(GROUPS)]
        if co2 != "all":
            base = base[base.co2 == co2]
        panel = {}
        for grp in GROUPS:
            g = base[base.adapt == grp]
            if g.empty:
                continue
            g = iqr_filter(g)
            pts = [[round(float(a), 2), round(float(b), 1), int(s), r] for a, b, s, r in
                   zip(g.dt, g.dp, g.study, g.Reference.fillna(""))]
            ent = {"n": len(g), "studies": int(g.study.nunique()), "pts": pts, "removed": int((base.adapt == grp).sum() - len(g))}
            if len(g) >= 10 and g.study.nunique() >= 2 and np.ptp(g.dt) >= 1.0:
                grid, preds = boot_fit(g)
                ent["curve"] = [[round(float(t), 2), round(float(m), 1), round(float(lo), 1), round(float(hi), 1)]
                                for t, m, lo, hi in zip(grid, preds.mean(0), np.percentile(preds, 2.5, 0),
                                                        np.percentile(preds, 97.5, 0))]
                ent["at"] = {str(t): at(grid, preds, t) for t in (1, 1.5, 2, 3, 4)}
            panel[grp] = ent
        fits[c][co2] = panel
    print(c, {k: len(v) for k, v in fits[c].items()}, flush=True)

# ---------- map outline (equirectangular, 0.1 deg) ----------
geo = json.load(open(GEO, encoding="utf-8"))
paths = []
for f in geo["features"]:
    gm = f["geometry"]
    polys = gm["coordinates"] if gm["type"] == "MultiPolygon" else [gm["coordinates"]]
    p = ""
    for poly in polys:
        for ring in poly:
            pts, last = [], None
            for lon, lat in ring:
                q = (round(lon, 1), round(-lat, 1))
                if q != last:
                    pts.append(q)
                    last = q
            if len(pts) > 3:
                p += "M" + "L".join(f"{a:g},{b:g}" for a, b in pts) + "Z"
    paths.append(p)

refs = (d.drop_duplicates("study")[["study", "Reference", "doi", "Publication year", "crop", "Methods"]])
refs_by = d.groupby("study").agg(crops=("crop", lambda s: sorted(s.unique().tolist())), rows=("ID", "size"))
refs = [{"id": int(r.study), "ref": r.Reference or "", "doi": r.doi if isinstance(r.doi, str) else "",
         "year": None if pd.isna(r._4) else int(r._4), "src": r.Methods,
         "crops": refs_by.loc[r.study, "crops"], "rows": int(refs_by.loc[r.study, "rows"])}
        for r in refs.itertuples()]

payload = {
    "kpi": {"rows": len(d), "studies": int(d.study.nunique()), "crops": int(d.crop.nunique()),
            "countries": int(d.country.dropna().loc[lambda s: ~s.isin(MULTI)].nunique()),
            "with_dt": int(d.dt.notna().sum()), "years": [int(d["Publication year"].min()), int(d["Publication year"].max())]},
    "crops": crops, "scen": xtab("scen", SCEN), "hor": xtab("hor", HOR), "ad": xtab("adapt", AD),
    "SCEN": SCEN, "HOR": HOR, "AD": AD, "sites": sites, "country": country, "models": models, "src": src,
    "fits": fits, "GROUPS": GROUPS, "map": paths, "refs": refs,
}
json.dump(payload, open(OUT, "w", encoding="utf-8"), ensure_ascii=False, separators=(",", ":"))
print("written", OUT)

# Optional: inject payload into the page template -> self-contained HTML
if len(sys.argv) > 5:
    tpl = open(sys.argv[4], encoding="utf-8").read()
    open(sys.argv[5], "w", encoding="utf-8").write(tpl.replace("__DATA__", json.dumps(payload, ensure_ascii=False, separators=(",", ":"))))
    print("page", sys.argv[5])
