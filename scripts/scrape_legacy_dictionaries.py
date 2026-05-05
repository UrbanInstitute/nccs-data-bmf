"""
Scrape NCCS legacy 501CX BMF data dictionaries.

Inputs:  hard-coded list of (year-month, url) from the catalog page
Outputs:
  - data/crosswalks/legacy_dictionaries_raw_html/<id>.html  (mirrored HTML)
  - data/crosswalks/legacy_dictionaries_raw/<id>.csv         (one CSV per dictionary)
  - data/crosswalks/legacy_column_inventory.csv              (long-format union)
  - data/crosswalks/legacy_dictionaries_index.csv            (one row per dictionary)
"""
import csv
import os
import re
import time
import urllib.request
from html import unescape
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
HTML_DIR = ROOT / "data/crosswalks/legacy_dictionaries_raw_html"
CSV_DIR = ROOT / "data/crosswalks/legacy_dictionaries_raw"
INVENTORY_PATH = ROOT / "data/crosswalks/legacy_column_inventory.csv"
INDEX_PATH = ROOT / "data/crosswalks/legacy_dictionaries_index.csv"

# (yyyy-mm, url). url == None means "data unavailable" placeholder; recorded but not fetched.
DICTIONARIES = [
    ("1989-06", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-1989-06-501CX-NONPROFIT-PX"),
    ("1995-08", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-1995-08-501CX-NONPROFIT-PX"),
    ("1996-06", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-1996-06-501CX-NONPROFIT-PX"),
    ("1997-10", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-1997-10-501CX-NONPROFIT-PX"),
    ("1998-09", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-1998-09-501CX-NONPROFIT-PX"),
    ("1999-12", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-1999-12-501CX-NONPROFIT-PX"),
    ("2000-05", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2000-05-501CX-NONPROFIT-PX"),
    ("2001-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2001-07-501CX-NONPROFIT-PX"),
    ("2002-01", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2002-01-501CX-NONPROFIT-PX"),
    ("2002-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2002-07-501CX-NONPROFIT-PX"),
    ("2003-01", None),
    ("2003-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2003-07-501CX-NONPROFIT-PX"),
    ("2003-11", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2003-11-501CX-NONPROFIT-PX"),
    ("2004-04", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2004-04-501CX-NONPROFIT-PX"),
    ("2004-12", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2004-12-501CX-NONPROFIT-PX"),
    ("2005-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2005-07-501CX-NONPROFIT-PX"),
    ("2005-11", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2005-11-501CX-NONPROFIT-PX"),
    ("2006-01", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2006-01-501CX-NONPROFIT-PX"),
    ("2006-05", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2006-05-501CX-NONPROFIT-PX"),
    ("2006-11", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2006-11-501CX-NONPROFIT-PX"),
    ("2007-01", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2007-01-501CX-NONPROFIT-PX"),
    ("2007-04", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2007-04-501CX-NONPROFIT-PX"),
    ("2007-09", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2007-09-501CX-NONPROFIT-PX"),
    ("2008-01", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2008-01-501CX-NONPROFIT-PX"),
    ("2008-04", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2008-04-501CX-NONPROFIT-PX"),
    ("2008-06", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2008-06-501CX-NONPROFIT-PX"),
    ("2008-10", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2008-10-501CX-NONPROFIT-PX"),
    ("2008-12", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2008-12-501CX-NONPROFIT-PX"),
    ("2009-01", None),
    ("2009-04", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2009-04-501CX-NONPROFIT-PX"),
    ("2009-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2009-07-501CX-NONPROFIT-PX"),
    ("2009-10", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2009-10-501CX-NONPROFIT-PX"),
    ("2010-01", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2010-01-501CX-NONPROFIT-PX"),
    ("2010-04", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2010-04-501CX-NONPROFIT-PX"),
    ("2010-05", None),
    ("2010-07", None),
    ("2010-08", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2010-08-501CX-NONPROFIT-PX"),
    ("2010-11", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2010-11-501CX-NONPROFIT-PX"),
    ("2011-06", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2011-06-501CX-NONPROFIT-PX"),
    ("2011-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2011-07-501CX-NONPROFIT-PX"),
    ("2011-08", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2011-08-501CX-NONPROFIT-PX"),
    ("2011-09", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2011-09-501CX-NONPROFIT-PX"),
    ("2011-10", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2011-10-501CX-NONPROFIT-PX"),
    ("2011-11", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2011-11-501CX-NONPROFIT-PX"),
    ("2011-12", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2011-12-501CX-NONPROFIT-PX"),
    ("2012-02", None),
    ("2012-03", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2012-03-501CX-NONPROFIT-PX"),
    ("2012-04", None),
    ("2012-06", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2012-06-501CX-NONPROFIT-PX"),
    ("2012-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2012-07-501CX-NONPROFIT-PX"),
    ("2012-08", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2012-08-501CX-NONPROFIT-PX"),
    ("2012-10", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2012-10-501CX-NONPROFIT-PX"),
    ("2012-11", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2012-11-501CX-NONPROFIT-PX"),
    ("2012-12", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2012-12-501CX-NONPROFIT-PX"),
    ("2013-02", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2013-02-501CX-NONPROFIT-PX"),
    ("2013-03", None),
    ("2013-04", None),
    ("2013-05", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2013-05-501CX-NONPROFIT-PX"),
    ("2013-06", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2013-06-501CX-NONPROFIT-PX"),
    ("2013-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2013-07-501CX-NONPROFIT-PX"),
    ("2013-08", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2013-08-501CX-NONPROFIT-PX"),
    ("2013-09", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2013-09-501CX-NONPROFIT-PX"),
    ("2013-10", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2013-10-501CX-NONPROFIT-PX"),
    ("2013-12", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2013-12-501CX-NONPROFIT-PX"),
    ("2014-02", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2014-02-501CX-NONPROFIT-PX"),
    ("2014-04", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2014-04-501CX-NONPROFIT-PX"),
    ("2014-06", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2014-06-501CX-NONPROFIT-PX"),
    ("2014-09", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2014-09-501CX-NONPROFIT-PX"),
    ("2014-11", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2014-11-501CX-NONPROFIT-PX"),
    ("2014-12", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2014-12-501CX-NONPROFIT-PX"),
    ("2015-02", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2015-02-501CX-NONPROFIT-PX"),
    ("2015-04", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2015-04-501CX-NONPROFIT-PX"),
    ("2015-05", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2015-05-501CX-NONPROFIT-PX"),
    ("2015-07", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2015-07-501CX-NONPROFIT-PX"),
    ("2015-09", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2015-09-501CX-NONPROFIT-PX"),
    ("2015-11", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2015-11-501CX-NONPROFIT-PX"),
    ("2015-12", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2015-12-501CX-NONPROFIT-PX"),
    ("2016-02", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2016-02-501CX-NONPROFIT-PX"),
    ("2016-03", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2016-03-501CX-NONPROFIT-PX"),
    ("2016-04", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2016-04-501CX-NONPROFIT-PX"),
    ("2016-08", "https://urbaninstitute.github.io/nccs-legacy/dictionary/bmf/bmf_archive_html/BMF-2016-08-501CX-NONPROFIT-PX"),
    ("2017-09", None),
    ("2017-12", None),
    ("2018-12", None),
    ("2019-08", None),
    ("2020-04", None),
    ("2022-01", None),
    ("2022-08", None),
]


FIELD_RE = re.compile(
    r'<td class="Data" valign="top">'
    r'<b>([^<]+)</b>'
    r'(?:<br>([A-Za-z]+))?'           # type
    r'(?:<br>\(([^)]+)\))?'           # length
    r'</td>'
    r'<td class="Data">'
    r'<b>([^<]*)</b>'                  # label
    r'<br>(.*?)'                       # description (lazy)
    r'(?:<table[^>]*class="Data"[^>]*>|</td>)',
    re.DOTALL,
)
SECTION_RE = re.compile(r'<th[^>]*class="PlaqueHeader"[^>]*>\s*<b[^>]*>([^<]+)</b>\s*</th>')
TITLE_RE = re.compile(r'<h1[^>]*>([^<]+)</h1>')
VARS_RE = re.compile(r'Number of variables</th><td[^>]*>\s*(\d+)\s*</td>')
RECS_RE = re.compile(r'Number of records</th><td[^>]*>\s*(\d+)\s*</td>')


def fetch(url: str, dest: Path) -> None:
    if dest.exists() and dest.stat().st_size > 0:
        return
    req = urllib.request.Request(url, headers={"User-Agent": "nccs-bmf-harmonization-scraper/1.0"})
    with urllib.request.urlopen(req, timeout=60) as resp:
        dest.write_bytes(resp.read())


def parse(html: str) -> tuple[dict, list[dict]]:
    title = TITLE_RE.search(html)
    nvars = VARS_RE.search(html)
    nrecs = RECS_RE.search(html)
    meta = {
        "title": title.group(1).strip() if title else "",
        "n_variables_declared": int(nvars.group(1)) if nvars else None,
        "n_records": int(nrecs.group(1)) if nrecs else None,
    }

    pos = 0
    current_section = ""
    fields: list[dict] = []
    while True:
        m_section = SECTION_RE.search(html, pos)
        m_field = FIELD_RE.search(html, pos)
        if not m_field:
            break
        if m_section and m_section.start() < m_field.start():
            current_section = m_section.group(1).strip()
            pos = m_section.end()
            continue
        name = m_field.group(1).strip()
        dtype = (m_field.group(2) or "").strip()
        length = (m_field.group(3) or "").strip()
        label = unescape(m_field.group(4)).strip()
        desc_html = m_field.group(5)
        desc = re.sub(r"<[^>]+>", " ", desc_html)
        desc = unescape(desc)
        desc = re.sub(r"\s+", " ", desc).strip()
        fields.append({
            "section": current_section,
            "name": name,
            "name_upper": name.upper(),
            "dtype": dtype,
            "length": length,
            "label": label,
            "description": desc,
        })
        pos = m_field.end()
    return meta, fields


def main() -> None:
    HTML_DIR.mkdir(parents=True, exist_ok=True)
    CSV_DIR.mkdir(parents=True, exist_ok=True)

    index_rows = []
    inventory_rows = []

    for ym, url in DICTIONARIES:
        dict_id = f"BMF-{ym}-501CX-NONPROFIT-PX"
        if url is None:
            index_rows.append({
                "dictionary_id": dict_id,
                "year_month": ym,
                "url": "",
                "status": "unavailable",
                "title": "",
                "n_variables_declared": "",
                "n_variables_parsed": 0,
                "n_records": "",
            })
            print(f"  [skip-unavailable] {ym}")
            continue

        html_path = HTML_DIR / f"{dict_id}.html"
        try:
            fetch(url, html_path)
        except Exception as e:
            print(f"  [fetch-error]      {ym}: {e}")
            index_rows.append({
                "dictionary_id": dict_id, "year_month": ym, "url": url,
                "status": f"fetch_error:{e}", "title": "",
                "n_variables_declared": "", "n_variables_parsed": 0, "n_records": "",
            })
            continue
        time.sleep(0.3)  # polite

        html = html_path.read_text(encoding="utf-8", errors="replace")
        meta, fields = parse(html)

        # Per-dictionary CSV
        csv_path = CSV_DIR / f"{dict_id}.csv"
        with csv_path.open("w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=["section", "name", "name_upper", "dtype", "length", "label", "description"])
            w.writeheader()
            w.writerows(fields)

        for fld in fields:
            inventory_rows.append({
                "column_name": fld["name"],
                "column_name_upper": fld["name_upper"],
                "dictionary_id": dict_id,
                "year_month": ym,
                "section": fld["section"],
                "dtype": fld["dtype"],
                "length": fld["length"],
                "label": fld["label"],
                "description": fld["description"],
            })

        index_rows.append({
            "dictionary_id": dict_id,
            "year_month": ym,
            "url": url,
            "status": "ok",
            "title": meta["title"],
            "n_variables_declared": meta["n_variables_declared"] or "",
            "n_variables_parsed": len(fields),
            "n_records": meta["n_records"] or "",
        })

        ok = (meta["n_variables_declared"] is None) or (meta["n_variables_declared"] == len(fields))
        flag = "OK " if ok else "MISMATCH"
        print(f"  [{flag}]            {ym}  declared={meta['n_variables_declared']} parsed={len(fields)}")

    # Write index
    with INDEX_PATH.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["dictionary_id", "year_month", "url", "status", "title",
                                           "n_variables_declared", "n_variables_parsed", "n_records"])
        w.writeheader()
        w.writerows(index_rows)

    # Write inventory
    with INVENTORY_PATH.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["column_name", "column_name_upper", "dictionary_id",
                                           "year_month", "section", "dtype", "length", "label", "description"])
        w.writeheader()
        w.writerows(inventory_rows)

    print()
    print(f"Dictionaries: {len(DICTIONARIES)} listed, "
          f"{sum(1 for r in index_rows if r['status']=='ok')} fetched OK, "
          f"{sum(1 for r in index_rows if r['status']=='unavailable')} unavailable, "
          f"{sum(1 for r in index_rows if r['status'].startswith('fetch_error'))} fetch errors")
    print(f"Total field-rows in inventory: {len(inventory_rows)}")
    print(f"Distinct column_name (case-sensitive):  {len({r['column_name'] for r in inventory_rows})}")
    print(f"Distinct column_name_upper:             {len({r['column_name_upper'] for r in inventory_rows})}")


if __name__ == "__main__":
    main()
