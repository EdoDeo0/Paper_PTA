"""
Independent Python re-implementation of the INDEX-CONSTRUCTION step of
Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R (the "ENVIRONMENTAL PROVISIONS
INDICES" block, lines ~448-785).

Purpose: cross-language replication per /audit skill, Step 2. Does NOT re-derive the
WB/TREND extraction, country-year expansion, or merge (already verified byte-identical
to the cached output by re-running the R script directly - see audit report). This script
takes the already-merged, short-code-renamed file (Merged_TREND_WB.csv, produced by the R
script BEFORE index construction) and independently recomputes every index column using
plain Python (no pandas, to avoid touching the environment), then diffs against
Merged_TREND_WB_Indices_Only.csv to verify the index-construction arithmetic itself.

Never modifies the author's original scripts or data.
"""
import csv
import math

MERGED = "Data/Merged/Merged_TREND_WB.csv"
EXPECTED = "Data/Merged/Merged_TREND_WB_Indices_Only.csv"


def to_num(v):
    if v is None or v == "" or v.upper() == "NA":
        return None
    return float(v)


def row_sum(row, cols):
    """rowSums(..., na.rm = TRUE): NA treated as 0."""
    total = 0.0
    for c in cols:
        v = to_num(row.get(c))
        if v is not None:
            total += v
    return total


def row_count_positive(row, cols):
    """rowSums(select(...) > 0, na.rm = TRUE)."""
    n = 0
    for c in cols:
        v = to_num(row.get(c))
        if v is not None and v > 0:
            n += 1
    return n


def n_available(row, cols):
    n = 0
    for c in cols:
        if to_num(row.get(c)) is not None:
            n += 1
    return n


def safe_div_round(num, den, ndigits=3, zero_if_den_zero=False):
    if den is None:
        return None
    if den > 0:
        return round(num / den, ndigits)
    return 0.0 if zero_if_den_zero else None


def main():
    with open(MERGED, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = list(reader)

    all_cols = rows[0].keys()
    wb_cols = [c for c in all_cols if c.startswith("WB_")]
    x_cols = [c for c in all_cols if c.startswith("X")]

    soft_cols = (
        [c for c in x_cols if c.startswith("X1_")] + ["X7_09", "X5_01_02"]
    )
    hard_raw_cols = (
        [c for c in x_cols if c.startswith("X2_")]
        + [c for c in x_cols if c.startswith("X5_")]
        + [c for c in x_cols if c.startswith("X10_")]
        + [c for c in x_cols if c.startswith("X14_")]
    )
    enforcement_cols = (
        [c for c in x_cols if c.startswith("X5_")]
        + [c for c in x_cols if c.startswith("X13_")]
        + [c for c in x_cols if c.startswith("X11_")]
        + [c for c in x_cols if c.startswith("X12_")]
    )
    regspace_cols = (
        ["X1_07_01", "X1_07_02", "X1_07_03", "X1_07_04",
         "X1_08_01", "X1_08_02", "X1_08_03", "X1_08_04",
         "X1_09_01", "X1_09_02"]
        + [c for c in x_cols if c.startswith("X8_")]
    )
    green_cols = ["X7_01_01", "X7_01_02_01", "X7_01_02_02", "X8_09_04"]
    climate_cols = ["X4_03"] + [c for c in x_cols if c.startswith("X10_")]
    biodiv_cols = ["X1_07_02", "X1_07_03"] + [c for c in x_cols if c.startswith("X11_")]

    wb_standards_cols = ["WB_2", "WB_8", "WB_9"]
    wb_enforcement_cols = ["WB_13", "WB_14", "WB_15", "WB_16"]
    wb_regspace_cols = ["WB_5", "WB_6", "WB_7"]

    computed = {}
    for row in rows:
        key = (row["country_code"], row["year"])

        trend_ep_count = row_sum(row, x_cols)
        trend_ep_count_bin = row_count_positive(row, x_cols)
        trend_soft = row_sum(row, soft_cols)
        trend_hard = max(row_sum(row, hard_raw_cols) - trend_soft, 0)
        trend_hardness_share = safe_div_round(trend_hard, trend_hard + trend_soft, zero_if_den_zero=True)
        trend_enforcement = row_sum(row, enforcement_cols)
        trend_regspace = row_sum(row, regspace_cols)
        trend_green = row_sum(row, green_cols)
        trend_climate = row_sum(row, climate_cols)
        trend_biodiv = row_sum(row, biodiv_cols)

        wb_depth = row_sum(row, wb_cols)
        wb_depth_bin = row_count_positive(row, wb_cols)
        wb_standards = row_sum(row, wb_standards_cols)
        wb_enforcement = row_sum(row, wb_enforcement_cols)
        wb_regspace = row_sum(row, wb_regspace_cols)
        wb_green = to_num(row.get("WB_10")) or 0.0
        wb_assist = to_num(row.get("WB_17")) or 0.0

        n_trend_avail = n_available(row, x_cols)
        n_wb_avail = n_available(row, wb_cols)

        computed[key] = {
            "TREND_EP_Count": trend_ep_count,
            "TREND_EP_Count_Binary": trend_ep_count_bin,
            "TREND_Soft": trend_soft,
            "TREND_Hard": trend_hard,
            "TREND_Hardness_Share": trend_hardness_share,
            "TREND_EnforcementDSM": trend_enforcement,
            "TREND_RegulatorySpace": trend_regspace,
            "TREND_GreenMarketAccess": trend_green,
            "TREND_ClimateEnergy": trend_climate,
            "TREND_BiodivForestsFisheries": trend_biodiv,
            "WB_EP_Depth": wb_depth,
            "WB_EP_Depth_Binary": wb_depth_bin,
            "WB_StandardsNonRegression": wb_standards,
            "WB_EnforcementDSM": wb_enforcement,
            "WB_RegulatorySpaceExceptions": wb_regspace,
            "WB_GreenLiberalization": wb_green,
            "WB_Assistance": wb_assist,
            "N_TREND_available": n_trend_avail,
            "N_WB_available": n_wb_avail,
            "TREND_Depth_Norm": safe_div_round(trend_ep_count, n_trend_avail),
            "WB_Depth_Norm": safe_div_round(wb_depth, n_wb_avail),
            "WB_Hardness_Share": safe_div_round(wb_standards, wb_depth),
            "TREND_Enforcement_Share": safe_div_round(trend_enforcement, trend_ep_count),
            "WB_Enforcement_Share": safe_div_round(wb_enforcement, wb_depth),
            "TREND_RegSpace_Share": safe_div_round(trend_regspace, trend_ep_count),
            "WB_RegSpace_Share": safe_div_round(wb_regspace, wb_depth),
            "TREND_GreenLib_Share": safe_div_round(trend_green, trend_ep_count),
            "WB_GreenLib_Share": safe_div_round(wb_green, wb_depth),
        }

    # Compare against the R-produced indices file
    with open(EXPECTED, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        expected_rows = list(reader)

    mismatches = []
    checked = 0
    for erow in expected_rows:
        key = (erow["country_code"], erow["year"])
        crow = computed.get(key)
        if crow is None:
            mismatches.append((key, "MISSING_IN_PYTHON_REPLICATION", None, None))
            continue
        for col, pyval in crow.items():
            rval_raw = erow.get(col)
            rval = None if rval_raw in (None, "", "NA") else float(rval_raw)
            checked += 1
            if rval is None and pyval is None:
                continue
            if rval is None or pyval is None:
                mismatches.append((key, col, rval, pyval))
                continue
            if not math.isclose(rval, pyval, abs_tol=1e-6):
                mismatches.append((key, col, rval, pyval))

    print(f"Rows compared: {len(expected_rows)}")
    print(f"Cell comparisons: {checked}")
    print(f"Mismatches (tol=1e-6): {len(mismatches)}")
    if mismatches:
        distinct_cols = sorted(set(m[1] for m in mismatches))
        print(f"\nDistinct columns with mismatches: {distinct_cols}")
        print("\nFirst 20 mismatches:")
        for m in mismatches[:20]:
            print(f"  key={m[0]} col={m[1]} R={m[2]} Python={m[3]}")
    else:
        print("ALL MATCH to 6 decimal places.")


if __name__ == "__main__":
    main()
