"""
Build: Environmental Provisions in Chinese PTAs — Working Paper Draft
Genera il PDF di stato del progetto per Edoardo Vitella.
Versione 2: contenuto più esteso, tabelle con Paragraph per word-wrap corretto.
"""

from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, PageBreak, Table, TableStyle,
    HRFlowable, KeepTogether
)
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import cm
from reportlab.lib import colors
from reportlab.lib.enums import TA_CENTER, TA_LEFT, TA_JUSTIFY, TA_RIGHT

OUTPUT = r"C:\Work\projects\Paper_PTA\New\Output\WorkingPaper_PTA_Status.pdf"

# ── Page geometry ────────────────────────────────────────────────────────────
PAGE_W = A4[0] - 5*cm   # usable width (left 2.5 + right 2.5)

# ── Styles ───────────────────────────────────────────────────────────────────
styles = getSampleStyleSheet()

C_DARK   = colors.HexColor("#1a3a5c")
C_MID    = colors.HexColor("#2c5f8a")
C_LIGHT  = colors.HexColor("#4a90c4")
C_RED    = colors.HexColor("#8b0000")
C_GREY   = colors.HexColor("#555555")
C_ROW1   = colors.HexColor("#eef2f7")
C_ROW2   = colors.white
C_GRID   = colors.HexColor("#aaaaaa")
C_THEAD1 = colors.HexColor("#1a3a5c")
C_THEAD2 = colors.HexColor("#2c5f8a")
C_THEAD3 = colors.HexColor("#3d7ab5")

title_style = ParagraphStyle("Title", parent=styles["Normal"],
    fontSize=22, leading=28, spaceAfter=10, alignment=TA_CENTER,
    fontName="Helvetica-Bold", textColor=C_DARK)

subtitle_style = ParagraphStyle("Subtitle", parent=styles["Normal"],
    fontSize=14, leading=19, spaceAfter=6, alignment=TA_CENTER,
    fontName="Helvetica", textColor=C_MID)

author_style = ParagraphStyle("Author", parent=styles["Normal"],
    fontSize=12, leading=17, spaceAfter=4, alignment=TA_CENTER,
    fontName="Helvetica-Oblique", textColor=C_DARK)

affiliation_style = ParagraphStyle("Affil", parent=styles["Normal"],
    fontSize=10, leading=14, spaceAfter=2, alignment=TA_CENTER,
    fontName="Helvetica", textColor=C_GREY)

date_style = ParagraphStyle("Date", parent=styles["Normal"],
    fontSize=10, leading=14, spaceAfter=20, alignment=TA_CENTER,
    fontName="Helvetica", textColor=C_GREY)

h1 = ParagraphStyle("H1", parent=styles["Normal"],
    fontSize=14, leading=19, spaceBefore=20, spaceAfter=8,
    fontName="Helvetica-Bold", textColor=C_DARK)

h2 = ParagraphStyle("H2", parent=styles["Normal"],
    fontSize=12, leading=16, spaceBefore=14, spaceAfter=6,
    fontName="Helvetica-Bold", textColor=C_MID)

h3 = ParagraphStyle("H3", parent=styles["Normal"],
    fontSize=11, leading=15, spaceBefore=10, spaceAfter=4,
    fontName="Helvetica-Bold", textColor=colors.HexColor("#333333"))

body = ParagraphStyle("Body", parent=styles["Normal"],
    fontSize=10, leading=15, spaceAfter=6, alignment=TA_JUSTIFY,
    fontName="Helvetica")

body_small = ParagraphStyle("BodySmall", parent=styles["Normal"],
    fontSize=9, leading=13, spaceAfter=5, alignment=TA_JUSTIFY,
    fontName="Helvetica")

bullet = ParagraphStyle("Bullet", parent=styles["Normal"],
    fontSize=10, leading=14, spaceAfter=3, leftIndent=18,
    bulletIndent=6, fontName="Helvetica")

sub_bullet = ParagraphStyle("SubBullet", parent=styles["Normal"],
    fontSize=9.5, leading=14, spaceAfter=2, leftIndent=34,
    bulletIndent=22, fontName="Helvetica", textColor=C_GREY)

code_style = ParagraphStyle("Code", parent=styles["Normal"],
    fontSize=8.5, leading=12, spaceAfter=4, leftIndent=14, rightIndent=14,
    fontName="Courier", backColor=colors.HexColor("#f5f5f5"),
    borderPadding=(4, 6, 4, 6))

note_style = ParagraphStyle("Note", parent=styles["Normal"],
    fontSize=9, leading=13, spaceAfter=4, alignment=TA_JUSTIFY,
    fontName="Helvetica-Oblique", textColor=C_GREY)

warning_style = ParagraphStyle("Warning", parent=styles["Normal"],
    fontSize=9.5, leading=14, spaceAfter=6, leftIndent=10,
    fontName="Helvetica-Bold", textColor=C_RED)

# ── Paragraph wrappers (for table cells) ────────────────────────────────────
_cell_body = ParagraphStyle("CellBody", parent=styles["Normal"],
    fontSize=9, leading=13, fontName="Helvetica",
    spaceAfter=0, spaceBefore=0)

_cell_head = ParagraphStyle("CellHead", parent=styles["Normal"],
    fontSize=9, leading=13, fontName="Helvetica-Bold",
    textColor=colors.white, spaceAfter=0, spaceBefore=0)

_cell_code = ParagraphStyle("CellCode", parent=styles["Normal"],
    fontSize=8.5, leading=12, fontName="Courier",
    spaceAfter=0, spaceBefore=0)

def pc(text):
    """Cell paragraph — body text for table cells."""
    return Paragraph(text, _cell_body)

def ph(text):
    """Header paragraph — white bold for dark header rows."""
    return Paragraph(text, _cell_head)

def pcc(text):
    """Code-style paragraph for table cells."""
    return Paragraph(text, _cell_code)

# ── Document helpers ─────────────────────────────────────────────────────────
def H(text, style=h1):   return Paragraph(text, style)
def P(text, style=body): return Paragraph(text, style)
def B(text):             return Paragraph(u"•  " + text, bullet)
def SB(text):            return Paragraph(u"◦  " + text, sub_bullet)
def SP(n=8):             return Spacer(1, n)
def HR():
    return HRFlowable(width="100%", thickness=0.5,
                      color=colors.HexColor("#cccccc"), spaceAfter=6)

# ── Generic table builder ────────────────────────────────────────────────────
def make_table(rows, col_widths, header_color=C_THEAD1,
               row_colors=None, font_size=9):
    """
    rows: list of lists; first row is the header.
    Each cell can be a string (auto-wrapped) or already a Paragraph/Flowable.
    """
    if row_colors is None:
        row_colors = [C_ROW1, C_ROW2]

    def wrap(cell, is_header):
        if isinstance(cell, str):
            return Paragraph(cell, _cell_head if is_header else _cell_body)
        return cell

    wrapped = []
    for i, row in enumerate(rows):
        wrapped.append([wrap(cell, i == 0) for cell in row])

    t = Table(wrapped, colWidths=col_widths, repeatRows=1)
    t.setStyle(TableStyle([
        # Header
        ("BACKGROUND",    (0, 0), (-1, 0), header_color),
        ("TEXTCOLOR",     (0, 0), (-1, 0), colors.white),
        ("FONTNAME",      (0, 0), (-1, 0), "Helvetica-Bold"),
        # Alternating rows
        ("ROWBACKGROUNDS",(0, 1), (-1, -1), row_colors),
        # Grid
        ("GRID",          (0, 0), (-1, -1), 0.3, C_GRID),
        # Alignment
        ("VALIGN",        (0, 0), (-1, -1), "TOP"),
        # Padding
        ("LEFTPADDING",   (0, 0), (-1, -1), 6),
        ("RIGHTPADDING",  (0, 0), (-1, -1), 6),
        ("TOPPADDING",    (0, 0), (-1, -1), 5),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 5),
        # Font size
        ("FONTSIZE",      (0, 1), (-1, -1), font_size),
    ]))
    return t

# ── Document ─────────────────────────────────────────────────────────────────
doc = SimpleDocTemplate(
    OUTPUT,
    pagesize=A4,
    leftMargin=2.5*cm, rightMargin=2.5*cm,
    topMargin=2.5*cm,  bottomMargin=2.5*cm,
    title="Environmental Provisions in Chinese PTAs — Working Paper Draft",
    author="Edoardo Vitella"
)

story = []

# ════════════════════════════════════════════════════════════════════════════
# TITLE PAGE
# ════════════════════════════════════════════════════════════════════════════
story += [
    SP(50),
    H("Environmental Provisions in Chinese Preferential Trade Agreements", title_style),
    SP(8),
    H("Do They Green China's Exports?", subtitle_style),
    SP(28),
    H("Working Paper — Project Status Report", subtitle_style),
    SP(4),
    H("DRAFT — For Internal Use Only", ParagraphStyle("Draft", parent=subtitle_style,
        textColor=C_RED, fontSize=11)),
    SP(36),
    H("Edoardo Vitella", author_style),
    H("PhD Candidate", affiliation_style),
    H("University of Trento & Free University of Bozen-Bolzano", affiliation_style),
    SP(6),
    H("June 2026", date_style),
    SP(50),
    HR(),
    SP(10),
    P("This document provides a comprehensive description of the project's research question, "
      "identification strategy, data, and current empirical status. It is intended as an "
      "orientation guide both for the author and for collaborators approaching the project "
      "for the first time. All analytical results reported here are preliminary and subject "
      "to revision.", note_style),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# ABSTRACT
# ════════════════════════════════════════════════════════════════════════════
story += [
    H("Abstract", h1),
    HR(),
    P("This paper investigates whether environmental provisions (EPs) in China's preferential "
      "trade agreements (PTAs) signed between 2000 and 2015 affect the composition of Chinese "
      "exports at the firm level. Using a transaction-level customs dataset covering approximately "
      "49 million firm-product-destination-year observations, combined with two independent "
      "indices of environmental provision depth — the World Bank Deep Trade Agreements (DTA) "
      "database and the TREND dataset (Morin et al. 2018) — we estimate the causal effect of "
      "EP depth on firms' exports of green and pollution-intensive goods.",
      ParagraphStyle("Abs", parent=body, leftIndent=20, rightIndent=20,
                     fontName="Helvetica-Oblique", fontSize=10.5)),
    SP(4),
    P("Our identification strategy exploits within-firm, within-destination, within-year "
      "variation across products of different environmental intensity, using a "
      "triple-difference design with a firm-destination-year fixed effect (fdt) that absorbs "
      "the entire entry-into-force of the agreement — tariff cuts, investment flows, trade "
      "facilitation — as a nuisance. Identification comes from the differential response of "
      "green versus dirty versus neutral products within the same firm-market-year cell, "
      "comparing markets where agreements have deeper environmental provisions.",
      ParagraphStyle("Abs", parent=body, leftIndent=20, rightIndent=20,
                     fontName="Helvetica-Oblique", fontSize=10.5)),
    SP(4),
    P("Preliminary diagnostic results show that the level effect of EP depth on aggregate "
      "exports is a precisely-estimated null — a pattern consistent with selection rather "
      "than a causal effect. The coefficient attenuates monotonically across progressively "
      "more saturated fixed-effect structures, and the significance found in the least-"
      "saturated specification is an artefact of under-clustering. The paper's key "
      "contribution is to test whether EPs reshape the composition of Chinese exports "
      "(green goods up, dirty goods down), using micro-level identification unavailable "
      "to prior country-level studies (Brandi et al. 2020; Abman, Lundberg & Ruta 2024).",
      ParagraphStyle("Abs", parent=body, leftIndent=20, rightIndent=20,
                     fontName="Helvetica-Oblique", fontSize=10.5)),
    SP(8),
    P("<b>Keywords:</b> environmental provisions, preferential trade agreements, China, "
      "green exports, dirty goods, firm-level customs data, triple-difference, "
      "pollution haven hypothesis, EP depth, TREND database, World Bank DTA", body_small),
    P("<b>JEL Codes:</b> F13, F18, Q56, Q58", body_small),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 1. INTRODUCTION
# ════════════════════════════════════════════════════════════════════════════
story += [H("1. Introduction and Research Question", h1), HR()]

story += [
    P("Preferential trade agreements (PTAs) have increasingly included environmental "
      "provisions (EPs) — ranging from general commitments to maintain domestic "
      "environmental laws to specific chapters on biodiversity, climate change, fisheries, "
      "and green market access. As of 2016, over 680 PTAs contained at least one "
      "environmental provision, and the average number of such provisions per agreement "
      "has grown substantially since the early 2000s (Morin, Dür & Lechner 2018). This "
      "proliferation raises a fundamental empirical question: are these provisions "
      "substantively consequential for trade flows, or are they largely declaratory?"),
    P("The question is particularly salient for China. As the world's largest goods "
      "exporter and a major source of embodied carbon in international trade, China's "
      "trade agreements have important environmental implications that extend well beyond "
      "its bilateral partners. Between 2000 and 2015, China concluded 14 PTAs with "
      "25 partner countries, several of which contain explicit environmental chapters — "
      "though their content and enforceability vary widely. Whether these provisions "
      "have any measurable effect on the environmental profile of Chinese exports is "
      "an open empirical question that this paper addresses for the first time using "
      "firm-level data."),
    P("The existing empirical literature on EPs and trade flows is thin and relies "
      "entirely on aggregate country-level data. Brandi et al. (2020, <i>World "
      "Development</i>) — the most direct predecessor — find that EPs in PTAs reduce "
      "the share of dirty exports and increase the share of green exports from developing "
      "countries, using a country-pair-year gravity panel spanning 1984–2016. Abman, "
      "Lundberg & Ruta (2024, <i>JEEA</i>) find that regional trade agreements with "
      "forest and biodiversity provisions effectively curb deforestation, using satellite "
      "data on land-use change. Both studies work at a level of aggregation that prevents "
      "them from disentangling the effect of EPs from the broader effects of trade "
      "liberalization, and neither can test for within-firm reallocation — arguably the "
      "most economically interesting margin."),

    H("1.1 Core Research Questions", h3),
    B("Do environmental provisions in China's PTAs shift the <i>composition</i> of Chinese "
      "exports towards greener goods and away from pollution-intensive goods? (The "
      "triple-difference question: the paper's central contribution.)"),
    B("Is the aggregate (level) effect of EP depth on Chinese exports a precisely-"
      "estimated null — and if so, does this reflect selection of higher-EP partners "
      "rather than a genuine causal effect? (The diagnostic/methodological contribution "
      "against Brandi 2020 and ALR 2024.)"),
    B("What is the role of within-firm reallocation: do multi-product firms shift their "
      "export mix towards green goods when serving EP-deep destinations, conditional on "
      "the overall level of their bilateral trade relationship?"),
    B("Which types of EP — green market access provisions, enforcement mechanisms, "
      "hard versus soft obligations — drive the effect, if any?"),
    SP(6),
    P("The paper's comparative advantage is the use of <b>firm-level Chinese customs "
      "data</b> covering approximately 49 million transactions at the firm-product-"
      "destination-year level. This granularity enables an identification strategy "
      "unavailable to prior work. The firm-destination-year fixed effect (fdt) absorbs "
      "everything that happens when an agreement enters into force — tariff cuts, "
      "investment liberalization, trade facilitation provisions, exchange rate effects — "
      "leaving only the differential effect of EP depth on green versus dirty versus "
      "neutral products within the same firm-market-year cell. This is the Rajan-Zingales "
      "(1998) logic applied to environmental provisions."),
    P("The answers carry implications both for the design of trade agreements and for "
      "the pollution-haven hypothesis applied to China. A null result — if precisely "
      "identified — would suggest that EPs in Chinese PTAs are largely declaratory rather "
      "than binding constraints on trade composition, an important finding for "
      "international environmental governance. A positive result would provide the first "
      "firm-level evidence that environmental treaty commitments reshape the micro-level "
      "structure of trade, beyond what aggregate studies can establish."),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 2. CHINA'S PTA LANDSCAPE AND ENVIRONMENTAL PROVISIONS
# ════════════════════════════════════════════════════════════════════════════
story += [H("2. China's PTA Landscape and Environmental Provisions", h1), HR()]

story += [
    P("China's engagement with preferential trade agreements accelerated significantly "
      "after its WTO accession in 2001. Prior to that, China had no PTAs in force. "
      "By 2015, it had concluded 14 agreements covering partners in Southeast Asia, "
      "South Asia, the Middle East, and Oceania. These agreements exhibit substantial "
      "heterogeneity in depth, scope, and environmental content."),
    SP(4),
    H("2.1 China's Agreements in the Sample (2000–2015)", h2),
    make_table([
        ["Agreement", "Entry in Force", "Partner Countries", "WB EP Depth (approx.)", "Notes"],
        ["ASEAN-China FTA (ACFTA)", "2005–2010",
         "Brunei, Cambodia, Indonesia, Laos, Malaysia, Myanmar, Philippines, Singapore, Thailand, Vietnam",
         "Low–Moderate", "Phased implementation; ASEAN as single agreement with 10 partners"],
        ["China–Pakistan FTA", "2007", "Pakistan", "Low", "Goods agreement; limited environmental chapter"],
        ["China–Chile FTA", "2006", "Chile", "Moderate", "Includes environment chapter; supplemented by 2012 upgrade"],
        ["China–New Zealand FTA", "2008", "New Zealand", "High", "Relatively comprehensive; NZ pushed for environmental text"],
        ["China–Singapore FTA", "2009", "Singapore", "Moderate", "Bilateral; overlaps with ACFTA"],
        ["China–Peru FTA", "2010", "Peru", "Moderate", "Includes environment chapter; Latin America"],
        ["China–Costa Rica FTA", "2011", "Costa Rica", "Moderate", "Smaller agreement; EP provisions unclear"],
        ["China–Iceland FTA", "2014", "Iceland", "Moderate–High", "Nordic partner; EU-influenced environmental text"],
        ["China–Switzerland FTA", "2014", "Switzerland", "High", "Comprehensive; includes specific EP obligations"],
        ["CEPA (HK & Macao)", "2003–2004", "Hong Kong, Macao", "—",
         "Sui generis; entrepôt hubs; excluded from main sample"],
    ], col_widths=[3.5*cm, 2.2*cm, 5.2*cm, 2.5*cm, 3.1*cm], header_color=C_THEAD2),
    SP(6),
    P("Note: ASEAN counts as a single agreement for identification purposes, yielding "
      "approximately <b>14 effective treatment clusters</b> (not 25 partner countries). "
      "This is important for inference: the effective degrees of freedom for identifying "
      "the level effect of EP depth are approximately 14, which severely limits power "
      "at the agreement level and motivates the shift to the within-firm product-composition "
      "strategy described in Section 5."),
    SP(6),
    H("2.2 Variation in EP Depth: The Two Indices", h2),
    P("This paper uses two independent measures of EP depth, which enables cross-validation "
      "and robustness checks. The indices differ in their coding methodology and scope "
      "but both aim to capture the substantive environmental ambition of China's agreements:"),
    SP(4),
    make_table([
        ["Dimension", "World Bank DTA Index", "TREND Dataset"],
        ["Source / Reference", "World Bank Deep Trade Agreements database (Hofmann-Osnago-Ruta)",
         "Morin, Dür & Lechner (2018), Global Environmental Politics"],
        ["Coverage", "52 types of provisions; 'Environmental Laws' area used",
         "286 types of EPs coded across 630+ PTAs worldwide"],
        ["Index construction", "Count of provisions in the Environmental Laws area; maximum across agreements for each destination-year",
         "Count of environmental provisions; dedicated EP module"],
        ["Granularity", "Sub-indices: GreenLiberalization, EnforcementDSM, StandardsNonRegression, RegulatorySpaceExceptions, Assistance, Hardness_Share",
         "Sub-indices: GreenMarketAccess, EnforcementDSM, RegulatorySpace, ClimateEnergy, BiodivForestsFisheries, Soft, Hard, Hardness_Share"],
        ["Variation level", "Destination × year (d,t)", "Destination × year (d,t)"],
        ["Advantage", "Harmonised with broader DTA depth measures; enables TotalDepth control",
         "More fine-grained EP taxonomy; independent coding — cross-validation"],
    ], col_widths=[3.5*cm, 6*cm, 6*cm], header_color=C_THEAD2),
    SP(6),
    P("Both indices are pre-merged into the final analysis dataset at the destination-year "
      "level and available as columns in the FST file. The correlation between WB_EP_Depth "
      "and TREND_EP_Count is high but imperfect, which is why we estimate all main "
      "specifications with both indices and require qualitative agreement between the two "
      "for any finding to be considered robust."),
    SP(6),
    H("2.3 The Identification Challenge Inherent in Level-Effect Designs", h2),
    P("Both indices share a fundamental limitation for level-effect identification: EP depth "
      "switches from zero to a positive value exactly when the agreement enters into force, "
      "and remains positive thereafter. This means that in a standard difference-in-"
      "differences design, the EP depth variable is perfectly collinear with the agreement "
      "entry dummy. The coefficient on EP depth captures the joint effect of <i>everything</i> "
      "that changes when the agreement enters into force — not specifically the environmental "
      "provisions. With only ~14 agreement-level variation units, separating the EP effect "
      "from the general liberalization effect is infeasible at the level. This is the "
      "fundamental reason why the triple-difference on product composition is the paper's "
      "main identification strategy."),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 3. DATA
# ════════════════════════════════════════════════════════════════════════════
story += [H("3. Data", h1), HR()]

story += [
    H("3.1 Chinese Customs Transaction Data", h2),
    P("The core dataset consists of Chinese export customs records for 2000–2015, "
      "obtained from the Chinese Customs General Administration and processed into "
      "a balanced panel of firm-product-destination-year observations. The raw data "
      "record every customs declaration, including exporter identity, HS6 product code, "
      "destination country, shipment value (USD), and quantity."),
    P("After cleaning, deduplication, and merging with the EP indices, the working "
      "dataset contains approximately <b>49.2 million observations</b>. It is stored "
      "as a compressed FST file (<i>final_dataset_pta_env_indices_compressed.fst</i>, "
      "approximately 14 GB on disk) enabling fast column-selective loading via the "
      "R <code>fst</code> package. The full dataset is never loaded into memory in a "
      "single R session; analysis scripts load only the columns required by the formula "
      "being estimated."),
    SP(6),
    H("3.2 Outcome Variables", h2),
    make_table([
        ["Variable", "Label in Dataset", "Description"],
        ["Log export value", "ln_export",
         "Natural log of total export value in USD for a firm-product-destination-year cell. "
         "Primary outcome. Captures the intensive margin of trade."],
        ["Log export quantity", "ln_export_qua",
         "Natural log of export quantity (physical units as declared in customs). "
         "Complements the value outcome: if both move together, the effect is on "
         "volume; if value moves more, there is a quality/price component."],
        ["Log unit value", "ln_export_value",
         "Natural log of unit value (USD per declared unit). Proxy for product quality "
         "and pricing. Note: unit values in raw customs data are noisy — trimming at the "
         "1st/99th percentile within HS2-year is planned (Fase R1)."],
    ], col_widths=[3*cm, 3.5*cm, 10*cm]),
    SP(6),
    H("3.3 Treatment Variables", h2),
    P("Treatment varies at the destination (d) × year (t) level. Both indices are "
      "available in the dataset as pre-merged columns:"),
    make_table([
        ["Variable", "Source", "Range", "Description"],
        ["WB_EP_Depth", "World Bank DTA", "0 – ~8",
         "Depth of environmental provisions in China's PTA with destination d, "
         "counting provisions in the Environmental Laws area. Zero before "
         "the agreement enters into force."],
        ["TREND_EP_Count", "TREND database", "0 – ~20",
         "Count of environmental provisions from the TREND dataset. "
         "Independent coding; used as cross-validation for WB_EP_Depth."],
        ["WB_TotalDepth", "World Bank DTA", "0 – ~50",
         "Total PTA depth (all provisions, not just environmental). "
         "Used as a control to separate EP effects from general depth effects. "
         "STILL TO BE CONSTRUCTED from WB DTA Excel files (Script 06)."],
    ], col_widths=[3.5*cm, 3*cm, 2*cm, 8*cm]),
    SP(6),
    H("3.4 Product-Level Environmental Classification", h2),
    P("The analysis requires classifying HS6 products by their environmental intensity. "
      "Two classifications are used (one available, one to be built):"),
    make_table([
        ["Classification", "Variable", "Source", "Status", "Description"],
        ["Green goods", "env_good", "OECD/APEC list",
         "Available in dataset",
         "Binary indicator: 1 if the HS6 product is on the combined "
         "OECD/APEC list of environmental goods (~142 HS6 codes, "
         "approximately 2.8% of world exports). Covers pollution-"
         "abatement equipment, clean energy goods, water treatment, "
         "air management equipment."],
        ["Dirty goods", "dirty_p", "Shapiro (2021) / ISIC",
         "Script written (05); pending execution",
         "Binary indicator: 1 if the product's industry is in the "
         "top quartile of CO2 intensity (emissions per USD of output). "
         "Source: Shapiro (2021) replication data (CO2 per industry), "
         "concorded ISIC → HS6 via R concordance package. Robustness: "
         "Mani-Wheeler (1998) industrial classification."],
    ], col_widths=[2.5*cm, 2*cm, 2.5*cm, 2.5*cm, 7*cm]),
    SP(6),
    P("Products not classified as green or dirty are treated as neutral. In the "
      "triple-difference design, the coefficients on EP_depth:green_p and "
      "EP_depth:dirty_p measure the differential export response of green and "
      "dirty products relative to neutral products within the same firm-destination-"
      "year cell."),
    SP(6),
    H("3.5 Pre-Computed Fixed-Effect Group Identifiers", h2),
    P("A distinctive feature of the dataset is that all fixed-effect group identifiers "
      "are pre-computed as integer IDs in the FST file, avoiding the need to "
      "recompute them at estimation time (which would be slow at 49M observations). "
      "The available FE IDs are:"),
    make_table([
        ["FE ID", "Levels", "Description"],
        ["fpd", "firm × product × destination",
         "Captures the time-invariant characteristics of the bilateral relationship at the "
         "product level: buyer-seller relationships, product-specific comparative "
         "advantage, distance and institutional costs, product-market fixed effects."],
        ["fpt", "firm × product × time",
         "Captures what a given firm does with a given product across all destinations "
         "in a given year: firm productivity shocks, product-level supply shocks, "
         "firm-product-specific technology changes."],
        ["fdt", "firm × destination × time",
         "KEY for the triple-diff design. Captures everything that happens to a given "
         "firm in a given destination in a given year: PTA entry, tariff cuts, "
         "demand shocks, exchange rates. Absorbs the entire treatment event."],
        ["pt", "product × time",
         "Global demand shocks for a product: solar panel boom, commodity price cycles, "
         "global value chain changes."],
        ["pd", "product × destination",
         "Time-invariant bilateral comparative advantage at the product level."],
        ["dt", "destination × time",
         "Destination-year effects: importer-side macro shocks, income growth, PTA entry (for all firms)."],
        ["pdt", "product × destination × time",
         "Most granular interaction: product-market-year shocks. Used in some robustness specs."],
        ["fp2dt", "firm × HS2 × destination × time",
         "Firm-chapter-destination-year: intermediate between fdt and fpt×dt."],
    ], col_widths=[2*cm, 3.5*cm, 11*cm]),
    SP(6),
    H("3.6 Control Variables", h2),
    make_table([
        ["Variable", "Description", "Status / Note"],
        ["tariffs (ln_duty)", "ln(1 + MFN duty rate) on HS6 product p in destination d in year t.",
         "AVAILABLE but MISCODED: this is the Most-Favoured Nation tariff, not the "
         "bilateral preferential rate applied to Chinese goods under the PTA. For PTA "
         "partner countries, the MFN rate does not decline after agreement entry. "
         "Replacing with preferential tariff from WITS TRAINS is Fase R2 priority."],
        ["tariffs_pref", "ln(1 + applied preferential tariff rate) — bilateral China-specific AHS.",
         "TO BE BUILT via Script 04 (WITS TRAINS API). Will replace ln_duty in "
         "the main triple-diff specification."],
        ["ln_hhi_baci", "Log of the Herfindahl-Hirschman Index for market concentration in product p × destination d.",
         "Available. Controls for market structure at the product-destination level."],
        ["AD_pdt", "Anti-dumping measure indicator: 1 if an anti-dumping measure is in force against China for product p in destination d in year t.",
         "Available. Important confounder: anti-dumping actions may coincide with PTA "
         "signing and may be more frequent for specific product categories."],
    ], col_widths=[2.5*cm, 5.5*cm, 7.5*cm]),
    SP(6),
    H("3.7 Data Still To Be Assembled", h2),
    make_table([
        ["Data Input", "Script", "Source / Method", "Priority"],
        ["Preferential tariffs\n(tariffs_pref_pdt)",
         "04_wits_pref_tariffs.R",
         "WITS TRAINS database via SDMX API. "
         "Reporter = PTA partner countries, partner = CHN (156), "
         "tariff type = AHS (preferential applied). "
         "Syntax verified: rest/data/DF_WITS_Tariff_TRAINS/A.{rep}.156.{hs6}.AHS.ALL. "
         "Caches results by file to avoid redundant API calls. "
         "Validation: tariff should decline after agreement entry for PTA partners.",
         "CRITICAL — without this, the tariff control is mis-specified"],
        ["Dirty goods\n(dirty_p)",
         "05_dirty_goods.R",
         "CO2 intensity per ISIC industry from Shapiro (2021) replication package "
         "(Harvard Dataverse). Concordance ISIC → HS6 via R concordance package. "
         "Top quartile of CO2 intensity = dirty. Robustness: Mani-Wheeler (1998) "
         "classification of pollution-intensive industries.",
         "CRITICAL — required for triple-diff"],
        ["TotalDepth\n(non-environmental)",
         "06_total_depth.R",
         "WB DTA Excel files already in repository (DTA 1.0 and 2.0). "
         "Count all provisions excluding the Environmental Laws area. "
         "Merge at destination-year level same as EP indices.",
         "HIGH — required to separate EP from general PTA depth"],
    ], col_widths=[3*cm, 3*cm, 7.5*cm, 3*cm]),
    SP(6),
    H("3.8 Dataset Scope and Limitations", h2),
    B("The dataset covers <b>export records only</b>. Import records and processing-"
      "trade regime flags are not available in the processed file, limiting GVC-type "
      "analyses such as imported input quality or backward GVC participation."),
    B("The <b>ownership type</b> (state-owned vs. private vs. foreign-invested enterprise) "
      "is not available in the processed dataset. It may be recoverable from the raw "
      "Stata file (<i>final_dataset_pta.dta</i>); this would enable an important "
      "heterogeneity analysis (SOEs may respond differently to treaty commitments)."),
    B("<b>Hong Kong and Macao</b> are in the dataset but will be excluded from the main "
      "sample. Both are entrepôt hubs — Chinese exports recorded as 'to HK' are often "
      "re-exports to third countries. The CEPA agreements with HK and Macao are also "
      "structurally different from China's other PTAs (no standard environmental chapter). "
      "They will be included as a robustness check."),
    B("The <b>HS concordance</b> across the three revision years within the 2000–2015 "
      "panel (HS 2002 → 2007 → 2012) is unverified. If codes are not concorded to a "
      "single vintage, the fpd and fpt fixed effects are artificially broken at revision "
      "boundaries. Script 02 will quantify this risk."),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 4. DIAGNOSTIC FINDINGS FROM THE ORIGINAL ANALYSIS
# ════════════════════════════════════════════════════════════════════════════
story += [H("4. Diagnostic Findings from the Original Analysis", h1), HR()]

story += [
    P("The project began with a standard gravity-style analysis — four fixed-effect "
      "structures estimated with OLS-HDFE, plus PPML and CEM robustness checks. "
      "A systematic audit of all 64 result tables (OLS + PPML × WB + TREND × 4–5 FE "
      "structures × full + CEM sample) revealed several important findings, summarized "
      "here. These diagnostics motivated the redesign described in Section 5."),
    SP(6),
    H("4.1 The Level Effect: Monotone Attenuation Across Fixed-Effect Structures", h2),
    P("The most important diagnostic finding is that the coefficient on EP depth (the "
      "'level effect' — beta on WB_EP_Depth or TREND_EP_Count in the baseline "
      "specification) attenuates monotonically as fixed-effect saturation increases:"),
    SP(4),
    make_table([
        ["FE Structure", "EP Depth Coef.", "Apparent Significance", "Clustering", "Interpretation"],
        ["fpd + year\n(least saturated)",
         "Positive, ~0.03–0.05",
         "2–3 stars on WB; 1–2 on TREND",
         "~pdt\n(~2.9M 'clusters')",
         "ARTEFACT. Clustering at pdt treats each firm-product-destination as an "
         "independent cluster — massively understates SEs. Treatment varies at (d,t) level, "
         "so correct clustering unit is country_code (~179 clusters, ~25 treated)."],
        ["fpt + pd",
         "Positive, ~0.01–0.02",
         "Reduced significance\n(1 star or less)",
         "~dt",
         "Adding more FEs absorbs part of the selection. Still positive but smaller. "
         "dt clustering is appropriate here."],
        ["fpt + fpd\n(preferred main spec)",
         "Near zero (~0.002)",
         "Not significant",
         "~country_code",
         "The fpd FE absorbs the time-invariant level of the bilateral relationship, "
         "including most of the selection. Coefficient near zero with correct clustering."],
        ["fpd + pt",
         "Near zero (~0.001)",
         "Not significant",
         "~country_code",
         "Alternative absorbing product-time shocks. Same conclusion: level effect is "
         "a precise null with correctly-saturated FEs."],
    ], col_widths=[3*cm, 2.5*cm, 2.5*cm, 2.5*cm, 5.5*cm]),
    SP(6),
    P("This monotone pattern is the <b>signature of positive selection</b>: "
      "destinations that sign deeper PTAs with China tend to be richer, faster-growing "
      "markets with more sophisticated demand. Firms that export to these markets would "
      "have increased their exports regardless of the EP content. The fpd fixed effect "
      "absorbs this time-invariant selection, removing the spurious correlation. The "
      "stars in the least-saturated specification were entirely driven by clustering at "
      "the wrong level — this is a clean example of the Bertrand-Duflo-Mullainathan "
      "(2004) under-clustering problem."),
    SP(4),
    P("This diagnostic finding is itself a publishable contribution: it provides the first "
      "firm-level evidence that the aggregate EP effects documented by Brandi et al. (2020) "
      "are likely driven by selection rather than causal identification. The 'ladder table' "
      "(OLS_Ladder_FE.tex) documenting this attenuation will be a key exhibit in the paper."),
    SP(6),
    H("4.2 The Interaction Effect: A Credible Signal", h2),
    P("One coefficient proved more stable across specifications: the interaction of EP depth "
      "with the green good indicator (<i>WB_EP_Depth × env_good</i>). Across several "
      "specifications, this was:"),
    make_table([
        ["Specification", "EP × env_good Coefficient", "Standard Error", "Notes"],
        ["fpt + fpd, WB, value, no controls", "+0.0009", "~0.0004",
         "Survives move to country_code clustering. Positive and stat. significant."],
        ["fpt + fpd, WB, quantity, no controls", "+0.0010", "~0.0004",
         "Similar magnitude. Effect on volume, not just price."],
        ["fpt + fpd, TREND, value, no controls", "+0.0011", "~0.0005",
         "TREND index confirms the direction. Slightly larger."],
        ["fpt + fpd, WB, value, with controls", "+0.0007", "~0.0005",
         "Attenuates slightly with controls but remains positive. Borderline."],
    ], col_widths=[5*cm, 3*cm, 2.5*cm, 6*cm]),
    SP(6),
    P("This positive interaction — deeper EP agreements are associated with "
      "differentially higher green exports — is the signal that the triple-difference "
      "design is intended to test rigorously. The magnitude is economically modest but "
      "the sign consistency across WB and TREND indices and across value/quantity "
      "outcomes is encouraging. The key question is whether this survives the "
      "full triple-diff specification with dirty_p as the counterfactual product group."),
    SP(6),
    H("4.3 Other Diagnostic Issues Identified", h2),
    B("<b>MFN tariff control is mis-specified.</b> The <code>duty</code> variable contains "
      "MFN tariff rates, not bilateral preferential rates. For PTA partner countries, the "
      "MFN rate shows no systematic decline after agreement entry (confirmed by time-series "
      "plots). This means the tariff coefficient — while the most stable in the original "
      "tables — is measuring the effect of the <i>external</i> tariff structure on "
      "export composition, not the PTA tariff preference. This is an important "
      "mis-specification for the mechanism: part of what we attribute to EP depth may "
      "be the correlation between EP provisions and deep tariff preferences."),
    B("<b>PPML internally inconsistent.</b> PPML estimates show sign reversals across "
      "outcomes (value positive, quantity sometimes negative) and across FE structures. "
      "The fpt-only PPML specification is a clear outlier with implausibly large "
      "coefficients — likely due to imperfect collinearity between the FE structure "
      "and the treatment variable when no bilateral relationship FE is included. "
      "PPML on unit values makes no conceptual sense (unit value is not a count/flow) "
      "and will be dropped. The two PPML robustness strategies to keep are described "
      "in Section 7.3."),
    B("<b>CEM matching is cosmetically weak.</b> 25 treated countries, 3 matching "
      "covariates (log GDP per capita, log GDP, year of agreement entry). L1 imbalance "
      "improves modestly from 0.788 to 0.652, but the standardized mean difference on "
      "log GDP per capita <i>worsens</i> after matching. With country-level treatment "
      "and 25 treated units, kernel matching on 3 covariates is cosmetic. The robust "
      "alternative is described in Section 7.4."),
    B("<b>Hong Kong and Macao contamination.</b> CEPA with HK and Macao is included in "
      "the treated group in the original analysis. Given HK's role as an entrepôt "
      "re-export hub and the structurally different nature of CEPA, these two "
      "destinations should be excluded from the main sample."),
    B("<b>Anti-dumping as confounder.</b> The AD_pdt variable is available but used "
      "inconsistently across the original specifications. Anti-dumping measures imposed "
      "on Chinese goods in PTA partner countries may correlate with both agreement "
      "signing and product composition — they are a natural confounder to include in "
      "the triple-diff."),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 5. REDESIGNED EMPIRICAL STRATEGY
# ════════════════════════════════════════════════════════════════════════════
story += [H("5. Redesigned Empirical Strategy", h1), HR()]

story += [
    H("5.1 The Core Identification Problem and Its Solution", h2),
    P("The fundamental identification challenge is that EP depth — both WB_EP_Depth and "
      "TREND_EP_Count — varies only at the destination × year level. It switches from "
      "zero to a positive value exactly when the agreement enters into force and remains "
      "positive thereafter. This means that EP depth is perfectly collinear with the "
      "<i>event</i> of PTA entry: any coefficient on EP depth alone estimates the joint "
      "effect of having a deep PTA (tariff cuts, investment provisions, trade facilitation, "
      "EP clauses) rather than the specific effect of environmental provisions."),
    P("With only ~14 effective agreements (the effective variation units), disentangling "
      "the EP contribution from the general liberalization effect is infeasible at the "
      "level-effect stage. Any serious referee at JIE or JEEM would reject the level "
      "effect as identification for EP-specific causal effects."),
    P("The solution is to abandon level-effect identification and instead exploit the "
      "<b>differential effect across products of different environmental intensity</b>. "
      "The hypothesis is that EP depth <i>specifically</i> reshapes export composition — "
      "green goods should increase relative to dirty and neutral goods — because "
      "environmental provisions create incentives for clean production and market access "
      "for environmental goods that do not apply uniformly across the product range. "
      "A general deep PTA should not differentially favor green over dirty products "
      "(unless it specifically includes green market access provisions — which is exactly "
      "what we are testing). The TotalDepth control separates these two channels."),
    SP(6),
    H("5.2 The Triple-Difference Specification", h2),
    P("The main estimating equation is:"),
    SP(4),
    P("ln_export(f,p,d,t) = β₁·EP(d,t)·green_p + β₂·EP(d,t)·dirty_p "
      "+ β₃·TotalD(d,t)·green_p + β₄·TotalD(d,t)·dirty_p "
      "+ γ·tariffspref(p,d,t) + δ·AD(p,d,t) "
      "+ α(f,p,d) + α(f,d,t) + α(p,t) + ε(f,p,d,t)",
      code_style),
    SP(4),
    P("Estimated in R using fixest:"),
    P("feols(ln_export ~ EP_depth:green_p + EP_depth:dirty_p\n"
      "               + TotalDepth:green_p + TotalDepth:dirty_p\n"
      "               + tariffs_pref + AD_pdt\n"
      "               | fpd + fdt + pt,\n"
      "      cluster = ~country_code,\n"
      "      data = dt)",
      code_style),
    SP(8),
    P("The key coefficients of interest are β₁ (EP × green) and β₂ (EP × dirty). "
      "The hypothesis consistent with EPs being effective is β₁ > 0 and β₂ < 0. "
      "β₃ and β₄ are controls for the general depth channel; if β₃ ≈ β₁ and β₄ ≈ β₂, "
      "it is the general depth rather than the EP content that drives the composition "
      "effect. If β₁ ≠ β₃ (specifically β₁ > β₃), this is evidence that it is "
      "specifically the environmental provisions that matter."),
    SP(6),
    H("5.3 Fixed Effects: What Each Absorbs", h2),
    make_table([
        ["Fixed Effect", "Notation", "Levels of Variation Absorbed", "Why It Matters"],
        ["Firm × Product × Destination\n(fpd)",
         "α(f,p,d)",
         "Time-invariant characteristics of the firm-product-destination triple: "
         "buyer-seller relationships, product-specific comparative advantage, "
         "iceberg transport costs, product-market entry costs.",
         "Removes the level of the bilateral relationship, "
         "which is the primary channel of selection (richer markets get "
         "more exports and deeper PTAs simultaneously)."],
        ["Firm × Destination × Year\n(fdt)",
         "α(f,d,t)",
         "EVERYTHING that varies for firm f in destination d in year t: "
         "PTA entry into force, all tariff cuts, all investment provisions, "
         "bilateral exchange rate, destination income growth, the EP depth "
         "level itself.",
         "The KEY identifying FE. With fdt in the model, the only "
         "residual variation is across products within the same "
         "firm-market-year cell. The EP level effect is absorbed. "
         "β₁ and β₂ are identified only from product heterogeneity."],
        ["Product × Year\n(pt)",
         "α(p,t)",
         "Global demand shocks for product p in year t: the solar panel boom, "
         "global commodity supercycle, GVC restructuring shocks that affect "
         "all exporters of product p.",
         "Ensures that green products are not trending globally for "
         "reasons unrelated to Chinese PTAs."],
    ], col_widths=[3.5*cm, 2*cm, 6.5*cm, 4.5*cm]),
    SP(6),
    H("5.4 Identification", h2),
    P("With the fdt fixed effect, the coefficients β₁ and β₂ are identified from "
      "variation <i>within</i> a firm-destination-year cell, <i>across</i> products "
      "of different environmental intensity. Formally, we are comparing:"),
    B("Do firms export <i>relatively more green</i> goods (and <i>relatively less dirty</i> "
      "goods) to destinations with deeper EP agreements, compared to what those same "
      "firms export to the same destinations in years before the agreement entered into "
      "force (or compared to their exports of neutral products)?"),
    SP(4),
    P("This is the Rajan-Zingales (1998) difference-in-differences across industries applied "
      "to environmental provisions. The identifying assumption is that, in the absence of "
      "EP provisions, the ratio of green to neutral exports would not systematically "
      "increase more in EP-deep destinations than in other destinations — conditional on "
      "all the variation absorbed by the three FEs."),
    P("This assumption is more credible than the level-effect parallel trends assumption "
      "because: (a) the fdt FE absorbs any differential trend in the level of bilateral "
      "exports; (b) the product-specific composition is determined by supply-side factors "
      "(technology, input costs) and demand-side factors (importer preferences) that do "
      "not obviously differentially affect green vs. dirty products for EP-deep "
      "destinations specifically."),
    SP(6),
    H("5.5 Pre-Trend Analysis and Event Study", h2),
    P("To validate the identification assumption, we estimate an event study around the "
      "entry into force of each agreement, looking for differential pre-trends in the "
      "green vs. neutral product ratio:"),
    P("feols(ln_export ~ i(rel_time, green_p, ref = -1)\n"
      "                + i(rel_time, dirty_p, ref = -1)\n"
      "               | fpd + fdt + pt,\n"
      "      cluster = ~country_code)",
      code_style),
    SP(4),
    P("where <code>rel_time</code> is years relative to agreement entry into force, "
      "ranging from -5 to +5 (saturated dummies). We expect: (a) pre-treatment "
      "coefficients on <code>i(rel_time, green_p)</code> to be near zero and not "
      "systematically increasing (parallel trends in composition); "
      "(b) post-treatment coefficients to diverge positively for green and negatively "
      "for dirty products if EPs are effective."),
    P("Since agreements entered into force at different times across the sample period "
      "(staggered design), we use <code>sunab()</code> from the fixest package to "
      "implement the Callaway-Sant'Anna (2021) / Sun-Abraham (2021) approach, which "
      "avoids the negative-weighting problem identified by Goodman-Bacon (2021) for "
      "heterogeneous-timing TWFE estimators."),
    SP(6),
    H("5.6 Inference Strategy: Three Layers", h2),
    P("With only ~14 effective treatment clusters (one per agreement), standard "
      "asymptotic cluster-robust SEs may be unreliable. We report three layers of "
      "inference for the main specification:"),
    make_table([
        ["Layer", "Method", "Implementation in R", "Rationale"],
        ["1 — Baseline",
         "Cluster-robust SEs at country_code level",
         "cluster = ~country_code\nin feols(); 179 clusters,\n~25 treated",
         "Asymptotically valid; reported as primary. Accounts for within-"
         "destination serial correlation. With 179 clusters total and ~25 "
         "treated, asymptotic theory is borderline reliable."],
        ["2 — Bootstrap",
         "Wild Cluster Bootstrap\n(B = 9,999)",
         "fwildclusterboot::\nboottest() on the\nfitted feols object",
         "More reliable than asymptotic clustering with few treated clusters "
         "(Cameron, Gelbach & Miller 2008). Imposes the null on the "
         "treatment variable; p-values from the bootstrap distribution."],
        ["3 — Permutation",
         "Permutation inference:\nrandomize EP depth\nover ~14 agreements",
         "1,000 random reassignments of EP depth vector across ~14 agreements; "
         "PTA timing fixed; compute t-stat each draw; exact p-value from distribution",
         "Exact p-value for the null that EP content is random, holding "
         "fixed the timing and existence of agreements. Tests specifically "
         "whether environmental content — not the agreement itself — "
         "drives the composition effect."],
    ], col_widths=[2*cm, 3.5*cm, 4*cm, 7*cm]),
    SP(6),
    P("We require robustness to all three inference methods for a result to be "
      "considered credible. In the paper, all three p-values will be reported in "
      "parentheses below the coefficients in the main table."),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 6. RELATED LITERATURE
# ════════════════════════════════════════════════════════════════════════════
story += [H("6. Related Literature", h1), HR()]

story += [
    H("6.1 Environmental Provisions in Trade Agreements", h2),
    P("The literature on EPs in trade agreements has developed rapidly, largely enabled "
      "by the TREND database (Morin et al. 2018), which codes 286 types of EPs across "
      "630+ PTAs. Key studies and their relevance:"),
    SP(4),
    make_table([
        ["Paper", "Journal / Outlet", "Main Finding", "Relevance to This Paper"],
        ["Brandi, Schwab, Berger & Morin (2020)",
         "World Development",
         "EPs in PTAs reduce the dirty export share and increase the green export share "
         "for developing countries. Country-pair-year gravity panel, 1984–2016. "
         "Controls: GDP, distance, colonial ties, RTA depth.",
         "Most direct predecessor. Our paper extends to firm level, focuses on China, "
         "and corrects the identification by absorbing the PTA entry through fdt FEs. "
         "The level-null against Brandi is itself a contribution."],
        ["Abman, Lundberg & Ruta (2024)",
         "JEEA (22:6)",
         "RTAs with forest/biodiversity provisions fully offset the deforestation caused "
         "by trade liberalization (satellite data). Effect is heterogeneous: only "
         "provisions with DSM (dispute settlement mechanisms) are effective.",
         "Closest top-journal precedent for EP causal identification. Key insight: "
         "enforcement matters (hard vs. soft provisions). We incorporate the hard/soft "
         "distinction in our sub-index heterogeneity analysis."],
        ["Morin, Dür & Lechner (2018)",
         "Global Environmental Politics",
         "Introduces the TREND dataset; documents the rapid proliferation of EPs across "
         "PTAs and the heterogeneity in their content. Shows that EP density increases "
         "with overall PTA ambition.",
         "Data source for the TREND_EP_Count index. The correlation between EP density "
         "and overall depth motivates the TotalDepth control (Fase R2)."],
        ["Neri-Laine, Orefice & Ruta (2023)",
         "CESifo WP",
         "Deep PTA provisions boost large-firm exports but hurt small firms' market "
         "entry. Firm-level EDD data. PPML with saturated FEs.",
         "Design template for our firm-size heterogeneity analysis (Fase R4). Also "
         "illustrates that aggregating across firm sizes masks heterogeneity."],
    ], col_widths=[3.5*cm, 2.5*cm, 5.5*cm, 5*cm]),
    SP(8),
    H("6.2 Trade, Pollution, and the Pollution Haven Hypothesis", h2),
    make_table([
        ["Paper", "Journal", "Main Finding", "Relevance"],
        ["Cherniwchan (2017)", "J. Intl. Econ.",
         "NAFTA tariff cuts caused within-plant reductions in PM10 and SO2 from US "
         "manufacturing. ~2/3 of the total US manufacturing emission decline 1994–98 "
         "is attributable to NAFTA. Demonstrates the technique effect at the micro level.",
         "Template for micro-level trade-environment evidence using differential "
         "product/industry exposure to the same agreement. Justifies the "
         "triple-difference design: the same logic — differential response across "
         "dirty vs. clean industries — applied to EP provisions."],
        ["Shapiro (2021)", "QJE",
         "Trade policy is systematically biased: tariffs are lower on dirty "
         "(upstream) industries globally, amounting to an implicit CO2 subsidy of "
         "$550–800bn/year. Driver: upstream location of dirty industries + lobbying.",
         "Source for the dirty_p measure (CO2 intensity per ISIC industry). "
         "Warning: preferential tariff cuts may be deeper for dirty goods — "
         "must be controlled in the triple-diff to avoid spurious β₂ estimates."],
        ["Copeland, Shapiro & Taylor (2022)", "Handbook of Intl. Econ.",
         "Comprehensive survey: theoretical framework decomposing trade effects into "
         "scale (↑ pollution via output), composition (shift toward cleaner industries), "
         "and technique effects (cleaner production at given output). Policy can affect "
         "all three margins.",
         "Theoretical framework for the paper. EP mechanisms: green market access "
         "affects composition margin; standards affect technique margin; both affect "
         "scale via trade volume."],
        ["Dechezleprêtre & Sato (2017)", "REEP",
         "Review: pollution haven effects exist but are modest; firm-level data "
         "reduce omitted variable bias substantially relative to industry-level studies.",
         "Motivates firm-level analysis. Provides context for expected effect sizes."],
        ["Mani & Wheeler (1998)", "J. Env. Dev.",
         "Identifies pollution-intensive industries for the 'dirty goods' classification: "
         "iron/steel, non-ferrous metals, industrial chemicals, pulp/paper, non-metallic minerals.",
         "Robustness classification for dirty_p alongside Shapiro (2021)."],
    ], col_widths=[3.5*cm, 2.5*cm, 5.5*cm, 5*cm]),
    SP(8),
    H("6.3 Identification and Econometric Methodology", h2),
    make_table([
        ["Paper", "Journal", "Main Finding", "Relevance"],
        ["Santos Silva & Tenreyro (2006)",
         "Rev. Econ. Stat.",
         "Log-linear gravity models are inconsistent under heteroskedasticity. "
         "PPML is consistent and has better small-sample properties.",
         "Justifies PPML robustness checks. Firm-level PPML on positive flows "
         "corrects heteroskedasticity; aggregate PPML handles zeros."],
        ["Goodman-Bacon (2021)",
         "J. Econometrics",
         "With staggered treatment timing, TWFE estimates are variance-weighted "
         "averages of 2×2 DiDs, some with negative weights (using later-treated "
         "units as controls for earlier-treated units).",
         "Motivates the sunab() approach in the event study. With 14 agreements "
         "at different entry times, standard TWFE event studies are unreliable."],
        ["Callaway & Sant'Anna (2021)",
         "J. Econometrics",
         "Proposes group-time average treatment effects that avoid the negative-"
         "weighting problem. Requires 'not-yet-treated' or 'never-treated' control groups.",
         "Implemented in Fase R5 as a robustness check on the binary treatment version. "
         "Requires splitting by agreement (group = entry year)."],
        ["de Chaisemartin & D'Haultfoeuille (2020)",
         "American Econ. Rev.",
         "Identifies conditions under which TWFE estimates can be negative even "
         "when all unit-level treatment effects are positive. Proposes heterogeneity-robust DiD.",
         "Same motivation as Goodman-Bacon. dCDH estimator as additional "
         "robustness check."],
        ["Cameron, Gelbach & Miller (2008)",
         "Rev. Econ. Stat.",
         "Wild cluster bootstrap provides more reliable inference than asymptotic "
         "cluster-robust SEs when the number of clusters is small.",
         "Justifies B=9,999 bootstrap. With ~14 treatment clusters, "
         "WCB is the primary inference method alongside permutation."],
        ["Bertrand, Duflo & Mullainathan (2004)",
         "QJE",
         "DiD SEs must account for within-group serial correlation; ignoring this "
         "massively understates SEs and overstates significance (spurious 60–70% "
         "rejection rate at 5% nominal level).",
         "Motivates correct clustering at country_code. The spurious stars in "
         "the fpd+year spec (clustered at pdt) are precisely this problem."],
        ["Larch, Shikher & Yotov (2025)",
         "Review of Intl. Econ.",
         "15 recommendations for applied gravity: PPML with zeros; complete bilateral "
         "pairs; time-varying FEs; within-country references; directional trade costs.",
         "Informs aggregate PPML specification: rectangular grid with zeros, "
         "FE pd + pt + dt, as recommended by Larch et al."],
    ], col_widths=[3.5*cm, 2.5*cm, 5.5*cm, 5*cm]),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 7. CURRENT STATUS
# ════════════════════════════════════════════════════════════════════════════
story += [H("7. Current Status of the Analysis", h1), HR()]

story += [
    P("The project is currently in <b>Fase R0</b>: completing the diagnostic inference "
      "exercise (re-estimation with correct clustering + wild bootstrap + ladder table) "
      "before moving to the main triple-difference analysis. The following tables "
      "document the completion status of all tasks across the roadmap phases."),
    SP(6),
    H("7.1 Fase R0 — Computational Progress: 01_inference_fix.R", h2),
    P("Script 01 re-estimates OLS models across four FE structures with uniform "
      "country_code clustering, wild bootstrap (B=9,999), and the ladder table. "
      "Each model on the fpt+fpd and fpd+pt structures takes approximately "
      "50–60 minutes at 10 threads (the regression involves ~49M observations "
      "with dual high-dimensional fixed effects). Status at time of writing:"),
    SP(4),
    make_table([
        ["FE Structure", "WB — No Int", "WB — Interactions", "TREND — No Int", "TREND — Int"],
        ["fpd + year", "6/6 DONE", "6/6 DONE", "6/6 DONE", "6/6 DONE"],
        ["fpt + pd",   "6/6 DONE", "6/6 DONE", "6/6 DONE", "6/6 DONE"],
        ["fpt + fpd",  "6/6 DONE", "6/6 DONE", "6/6 DONE", "RUNNING"],
        ["fpd + pt",   "PENDING",  "PENDING",  "PENDING",  "PENDING"],
    ], col_widths=[4*cm, 3*cm, 3*cm, 3.5*cm, 3.5*cm], header_color=C_THEAD3),
    SP(4),
    P("Note: 'No Int' = specification without interaction terms (level effect only). "
      "'Interactions' = specification with EP_depth × env_good and EP_depth × TotalDepth "
      "interaction terms. Each cell = 6 models (3 outcomes × {baseline, controls}). "
      "Total: 48 models across all FE structures. Currently ~36/48 complete. "
      "Remaining estimated computation time: 1–2 days."),
    SP(4),
    P("After all 48 base models complete: wild cluster bootstrap (B=9,999) will run "
      "on the fpt+fpd specification (the 'preferred' structure) for the key "
      "interaction coefficients. Then the ladder table (OLS_Ladder_FE.tex) will "
      "be generated programmatically from the stored RDS files."),
    SP(8),
    H("7.2 Full Roadmap: All Phases", h2),
    make_table([
        ["Phase", "Status", "Description", "Key Deliverable"],
        ["R0 — Close Diagnostic",
         "IN PROGRESS",
         "Complete 01_inference_fix.R: remaining OLS models (fpd+pt section, ~24 models), "
         "wild cluster bootstrap (B=9,999 on fpt+fpd), ladder table. One R process "
         "at a time to avoid the OpenMP allocator crash.",
         "bootstrap_summary.csv confirming null on level; "
         "OLS_Ladder_FE.tex with 4 rows (monotone attenuation)."],
        ["R1 — Data Hygiene",
         "PENDING",
         "Run 02_data_hygiene_audit.R. Five checks: (1) HS concordance across "
         "2002/2007/2012 revisions — CRITICAL, could invalidate existing results; "
         "(2) treatment table: 14 agreements, switch dates, EP depth values; "
         "(3) HK+Macao weight in the treated sample; "
         "(4) UV outlier trimming 1%/99% within HS2-year; "
         "(5) firm consistency check around 2004 (WTO liberalisation of trading rights).",
         "Diagnostic report in New/Output/Diagnostics/. "
         "Decision documented: concordance needed yes/no."],
        ["R2 — New Data",
         "PENDING\n(scripts written)",
         "Run 04 (WITS tariffs), 05 (dirty goods), 06 (TotalDepth). "
         "Scripts written and verified; need to execute. "
         "04 can run in parallel to R1 (only requires internet access, no FST). "
         "05 and 06 are fast (small datasets).",
         "tariffs_pref_pdt.csv, dirty_goods_hs6.csv, "
         "total_depth_dt.csv — all merge-validated."],
        ["R3 — Main Triple-Diff",
         "PENDING",
         "Run 07_triple_diff.R. Three sections: "
         "(A) main triple-diff estimates — 3 outcomes × {WB, TREND} with all three "
         "inference methods (cluster + WCB + permutation); "
         "(B) event study with sunab() for staggered design; "
         "(C) 1,000-draw permutation test. Also: PPML firm-level on positive flows "
         "(same triple-diff spec with fepois) + PPML aggregate pd×t with zeros.",
         "Table_TripleDiff_Main.tex; EventStudy plot; "
         "permutation_pvalues.csv."],
        ["R4 — Margins and Mechanisms",
         "PENDING",
         "Extensive margin: n firms / n products green per d×t cell. "
         "Within-firm reallocation: green share of multi-product firms' export basket "
         "toward d (identified by fdt FE — the within-firm variation is key). "
         "EP sub-index heterogeneity: GreenMarketAccess, EnforcementDSM, Hard/Soft. "
         "Firm size heterogeneity (terziles of total export value).",
         "Extensive margin table; within-firm green share table; "
         "sub-index table."],
        ["R5 — Robustness",
         "PENDING",
         "Leave-one-out per agreement (14 specifications). "
         "Exclude ASEAN bloc. Include/exclude HK+Macao. "
         "Not-yet-treated only as control group. "
         "Callaway-Sant'Anna on binary treatment version. "
         "Synthetic DiD on destination-level green export share. "
         "UV trimmed vs. raw. WB vs. TREND in all specs.",
         "Appendix tables; discussion of sensitivity."],
        ["R6 — Framing and Writing",
         "PENDING",
         "Framing decision after R3 results are available. "
         "Descriptive section: position China's EP agreements in the global TREND "
         "distribution (shallow vs. deep; timeline; EU/US comparison). "
         "Reduce from 48+ tables to 6–8 main tables + structured appendix. "
         "Target journal selection.",
         "Paper draft; target journal decision."],
    ], col_widths=[2.5*cm, 2.2*cm, 9.3*cm, 3*cm]),
    SP(8),
    H("7.3 Scripts in the New/ Directory", h2),
    make_table([
        ["Script", "Phase", "Status", "Description"],
        ["01_inference_fix.R", "R0", "RUNNING",
         "Four FE structures × {WB, TREND} × {No-Int, Int} × {baseline, controls}, "
         "all clustered at country_code. Uses callr::r() to serialize each section "
         "as a subprocess (avoids OpenMP/FST allocator conflict on Windows). "
         "Wild cluster bootstrap section. Ladder table generation."],
        ["02_data_hygiene_audit.R", "R1", "WRITTEN — ready",
         "HS concordance audit (count dying/new codes at each revision boundary), "
         "treatment table (14 agreements × dates/depth), HK+Macao export weight, "
         "UV trimming flags, firm consistency check around 2004."],
        ["04_wits_pref_tariffs.R", "R2", "WRITTEN — ready",
         "Preferential tariff download from WITS TRAINS via SDMX API. "
         "Loops over PTA partner countries, downloads AHS rates for China (partner 156), "
         "HS6 level, 2000–2015. Syntax verified against WITS SDMX endpoint. "
         "File-based cache to avoid redundant API calls."],
        ["05_dirty_goods.R", "R2", "WRITTEN — ready",
         "Dirty goods classification. Loads Shapiro (2021) CO2 intensity data "
         "(ISIC industry level). Concordance ISIC→HS6 via R concordance package. "
         "Top quartile = dirty. Robustness: Mani-Wheeler (1998) industry list. "
         "Output: dirty_goods_hs6.csv with dirty_p indicator."],
        ["06_total_depth.R", "R2", "WRITTEN — ready",
         "TotalDepth (all PTA provisions excluding environmental). Reads WB DTA "
         "Excel files from repository. Constructs total provision count and non-"
         "environmental count. Merges at destination-year level. "
         "Validation: correlation with existing WB_EP_Depth."],
        ["07_triple_diff.R", "R3", "WRITTEN — ready",
         "Full triple-difference: Section A (main OLS estimates, 3 outcomes × "
         "{WB,TREND}, all three inference methods), Section B (event study with sunab()), "
         "Section C (1,000-draw permutation inference). "
         "Requires: dirty_p, TotalDepth, tariffs_pref from Fase R2."],
    ], col_widths=[4*cm, 1.5*cm, 2*cm, 9*cm]),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 8. KEY DECISIONS AND OPEN QUESTIONS
# ════════════════════════════════════════════════════════════════════════════
story += [H("8. Key Pending Decisions and Open Questions", h1), HR()]

story += [
    H("8.1 Critical Decision: HS Concordance", h2),
    P("<b>This is the most urgent unresolved issue.</b> The panel covers 2000–2015, "
      "crossing three HS revision years: 2002 (HS1996 → HS2002), 2007 (HS2002 → "
      "HS2007), and 2012 (HS2007 → HS2012). At each revision, a subset of HS6 codes "
      "are split, merged, or renumbered. If the customs data have not been concorded "
      "to a single HS vintage, two problems arise:"),
    B("The firm-product-destination fixed effects (fpd) are <i>artificially broken</i> "
      "at revision boundaries: a product that existed continuously under one code is "
      "treated as two different products before and after reclassification. This "
      "introduces spurious entry and exit in the panel."),
    B("The env_good indicator — which is mapped to a specific HS vintage — is "
      "<i>incorrectly assigned</i> for portions of the sample: a green product may "
      "be classified as neutral before a revision, or vice versa."),
    SP(4),
    P("The audit script (02_data_hygiene_audit.R) will quantify the share of HS6 codes "
      "that change across each revision boundary and the fraction of total export value "
      "they represent. Decision rule:"),
    make_table([
        ["Scenario", "Action Required"],
        ["< 1% of export value affected by revision discontinuities",
         "No action required. Document and note as minor limitation."],
        ["1–3% of export value affected",
         "Partial concordance: remap affected codes only. Re-verify env_good mapping. "
         "Note as limitation with sensitivity check excluding revision-boundary years."],
        ["> 3% of export value affected",
         "Full concordance required: rebuild pipeline Steps 2–3 using UNSD concordance "
         "tables (HS1996–HS2002–HS2007–HS2012). This is several weeks of work "
         "but is essential for publishability. New FST file will be required."],
    ], col_widths=[5*cm, 11.5*cm], header_color=C_THEAD2),
    SP(6),
    H("8.2 Framing Decision: After Fase R3", h2),
    P("The ultimate framing and target journal depend on the triple-difference results. "
      "Two scenarios and their implications:"),
    SP(4),
    make_table([
        ["Scenario", "Result Pattern", "Framing", "Target Journal"],
        ["A — Signal survives",
         "β₁ (EP × green) > 0 and β₂ (EP × dirty) < 0, both significant under "
         "all three inference methods. Event study: no pre-trends, clear post-treatment "
         "divergence in green/dirty ratio. Permutation p < 0.05.",
         "Title: 'Do Environmental Provisions Green Exports? Firm-Level Evidence from China'. "
         "Headline: triple-diff composition effect + within-firm green reallocation "
         "(Fase R4). Level null as methodological contribution against Brandi (2020). "
         "The within-firm reallocation result would be novel and compelling.",
         "JIE or JEEM (full paper).\nPotentially REStud/AEJ:EP\nif within-firm result is strong."],
        ["B — Precise null",
         "Neither β₁ nor β₂ survives the more conservative inference methods. "
         "Permutation p > 0.10. Event study shows no differential pre/post pattern.",
         "Title: 'Are Environmental Provisions in Chinese PTAs Effective? "
         "A Micro-Level Null'. "
         "Contribution: first firm-level test; ladder documenting selection in aggregate "
         "studies; methodological critique of Brandi (2020) and ALR (2024); "
         "permutation test ruling out EP content as driver.",
         "World Development\nor JEEM (short paper).\nStill publishable as a methodological contribution."],
        ["C — Mixed",
         "β₁ (green) survives but β₂ (dirty) does not, or vice versa. "
         "Or: survives WCB but not permutation.",
         "Framing depends on which sub-result is more robust. Focus on the "
         "green market access sub-index if GreenMarketAccess drives β₁. "
         "Discuss dirty goods identification uncertainty as a caveat.",
         "JEEM or similar environmental economics journal."],
    ], col_widths=[2*cm, 5*cm, 6*cm, 3.5*cm]),
    SP(6),
    H("8.3 PPML Strategy", h2),
    P("Two PPML estimations are planned, serving different purposes:"),
    make_table([
        ["PPML Type", "Specification", "Purpose", "R Implementation"],
        ["Firm-level PPML\n(positive flows only)",
         "Same triple-diff as OLS: fepois(ln_export_raw ~ EP:green + ... | fpd + fdt + pt)\nOn strictly positive export flows only.",
         "Corrects for heteroskedasticity in the intensive margin "
         "(Santos Silva & Tenreyro 2006). Directly comparable to OLS as "
         "robustness check. Firms × products × destinations with zero exports in a "
         "given year are excluded.",
         "feols() → fepois() drop-in. Note: fepois does not accept log outcome; "
         "use raw export value. R² is meaningless in PPML — will not be reported."],
        ["Aggregate PPML\n(with zeros)",
         "Collapse to product × destination × year: fepois(export_raw ~ EP:green + ... | pd + pt + dt)\nOn rectangular grid including zero-trade cells.",
         "Captures the extensive margin: whether EP depth induces new product-market "
         "pairs to start trading. Comparable to Brandi (2020) and Larch et al. (2025). "
         "The 'true' PPML as recommended in the gravity literature.",
         "Aggregate to pdyt grid (~14M cells). fepois with FE pd + pt + dt. "
         "Large but feasible computation."],
    ], col_widths=[3*cm, 5.5*cm, 5.5*cm, 2.5*cm]),
    SP(4),
    P("Note: PPML on unit values (ln_export_value) makes no conceptual sense — unit "
      "value is not a count/flow variable and PPML does not apply. All original PPML "
      "specifications on unit values will be dropped."),
    SP(6),
    H("8.4 CEM: Appendix or Removal?", h2),
    P("The original CEM matching is methodologically weak:"),
    B("25 treated countries, 3 matching covariates (log GDP, log GDP per capita, "
      "year of entry). At the country level with limited degrees of freedom, "
      "kernel matching on 3 covariates is cosmetic."),
    B("L1 imbalance improves from 0.788 to 0.652 — modest. More importantly, "
      "the standardized mean difference on log GDP per capita <i>worsens</i> "
      "after matching, indicating that CEM creates imbalance on some important "
      "dimensions while improving others."),
    B("The matched control sample retains only a subset of the original control "
      "observations, reducing efficiency without credible identification gains."),
    SP(4),
    P("The plan is to <b>demote CEM to an appendix</b> (or remove entirely) and "
      "replace the robustness strategy with: (a) not-yet-treated destinations as "
      "the control group; (b) synthetic DiD at the destination level on the green "
      "export share; (c) Callaway-Sant'Anna on the binary treatment version of "
      "EP depth (above/below median EP depth conditional on having a PTA)."),
    SP(6),
    H("8.5 Zotero Collection: Pending Items", h2),
    P("Four key papers should be added to the Zotero Paper_PTA collection but are "
      "currently missing (Zotero was in local-only mode when last checked):"),
    make_table([
        ["Paper", "DOI", "Priority", "Why Missing"],
        ["Abman, Lundberg & Ruta (2024) JEEA",
         "10.1093/jeea/jvae023",
         "CRITICAL — closest competitor",
         "Zotero add-by-DOI failed in previous session (local-only mode)."],
        ["Shapiro (2021) QJE",
         "10.1093/qje/qjaa042",
         "CRITICAL — dirty_p source",
         "Same issue."],
        ["Cherniwchan (2017) JIE",
         "10.1016/j.jinteco.2017.01.005",
         "High",
         "Same issue."],
        ["Copeland, Shapiro & Taylor (2022) Handbook",
         "10.1016/bs.hesint.2022.02.002",
         "High",
         "Same issue."],
    ], col_widths=[4.5*cm, 4.5*cm, 3*cm, 4.5*cm], header_color=C_THEAD2),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 9. HETEROGENEITY ANALYSES (FASE R4)
# ════════════════════════════════════════════════════════════════════════════
story += [H("9. Planned Heterogeneity Analyses", h1), HR()]

story += [
    P("Once the main triple-difference results are established (Fase R3), the following "
      "heterogeneity analyses will be conducted. These are organized by what type of "
      "additional variation they exploit."),
    SP(6),
    H("9.1 Extensive Margin of Green Exports", h2),
    P("The main triple-difference operates on the intensive margin: export value/quantity "
      "conditional on positive trade. The extensive margin asks: does EP depth induce "
      "firms to start exporting green goods to a destination, or induce new products to "
      "enter new markets? We will estimate:"),
    B("<b>Firm-level extensive margin:</b> number of distinct firms that export at least "
      "one green product to destination d in year t. Regression of log(n_firms_green_dt) "
      "on EP_depth_dt with FE dt (or destination + year + their interaction). Compare "
      "growth in green exporters to growth in dirty/neutral exporters in treated vs. "
      "control destinations."),
    B("<b>Product-level extensive margin:</b> number of distinct HS6 green products "
      "exported to d in year t. Same design."),
    B("<b>Product entry:</b> for green products p not previously exported to d by firm f, "
      "does the year of first export shift toward the agreement entry year?"),
    SP(6),
    H("9.2 Within-Firm Green Share Reallocation", h2),
    P("This is potentially the strongest result in the paper, and novel relative to all "
      "prior literature. For multi-product firms (which account for most of Chinese "
      "exports by value), does the <i>share of green products in the firm's export "
      "basket toward destination d</i> increase after the agreement enters into force?"),
    P("Specification (Rajan-Zingales applied within multi-product firms):"),
    P("green_share(f,d,t) = beta * EP_depth(d,t) + FE(f,d) + FE(f,t) + epsilon",
      code_style),
    SP(4),
    P("where green_share(f,d,t) = [value of green exports from f to d in t] / "
      "[total exports from f to d in t]. The FE f×d absorbs the firm-destination-"
      "specific level; the FE f×t absorbs the firm-year trend in green specialization. "
      "The coefficient beta identifies whether a firm shifts its product mix toward green "
      "goods specifically in destinations where it faces deeper EP agreements, controlling "
      "for any general trend in the firm's green intensity."),
    P("This is identified from firms that export to <i>both</i> EP-deep and EP-shallow "
      "destinations simultaneously, allowing a within-firm cross-destination comparison. "
      "No such test has been done in the literature."),
    SP(6),
    H("9.3 EP Sub-Index Heterogeneity", h2),
    P("Not all types of environmental provisions are equally likely to affect trade "
      "composition. Theory suggests a hierarchy:"),
    make_table([
        ["Sub-Index", "Mechanism", "Expected Relevance", "Available in Dataset?"],
        ["GreenMarketAccess (TREND)\n/ GreenLiberalization (WB)",
         "Provisions specifically reducing tariffs and non-tariff barriers for "
         "environmental goods. Directly incentivizes green exports.",
         "HIGHEST — this is the most direct trade-composition mechanism. "
         "If this sub-index drives β₁, the interpretation is clean market access.",
         "Yes (both indices)"],
        ["EnforcementDSM\n(both indices)",
         "Provisions establishing dispute settlement mechanisms for environmental "
         "obligations. Abman et al. (2024) find this is the effective provision type.",
         "HIGH — enforceable provisions with DSM are more binding. "
         "Hard provisions without DSM may be cheaper to circumvent.",
         "Yes (both indices)"],
        ["Hard vs. Soft obligations\n(Hardness_Share)",
         "Share of provisions that are binding ('shall') vs. aspirational ('should', "
         "'may'). Binding provisions create legal obligations.",
         "MODERATE — consistent with ALR (2024) finding that hard provisions are "
         "what matters for deforestation.",
         "Yes (as share; requires construction of hard-only sub-index)"],
        ["StandardsNonRegression (WB)\n/ RegulatorySpace (TREND)",
         "Provisions preventing countries from weakening environmental standards "
         "to attract trade. Pollution haven suppression mechanism.",
         "MODERATE — directly relevant to dirty goods: if firms cannot exploit "
         "lax environmental standards, dirty exports may be constrained.",
         "Yes (WB has this; TREND equivalent available)"],
    ], col_widths=[4*cm, 5.5*cm, 4*cm, 3*cm]),
    SP(6),
    H("9.4 Firm-Size Heterogeneity", h2),
    P("Following Neri-Laine, Orefice & Ruta (2023), we expect heterogeneous effects "
      "by firm size: large, established exporters may more easily adapt to EP-driven "
      "demand for green goods (they already have compliance infrastructure), while "
      "small firms may face higher fixed costs of product adaptation. We split the "
      "sample into terciles by total export value (or employment, if available) and "
      "estimate the triple-diff separately:"),
    B("Large firms: above 66th percentile of total annual exports."),
    B("Medium firms: 33rd–66th percentile."),
    B("Small firms: below 33rd percentile."),
    SP(4),
    P("We expect β₁ to be larger in magnitude for large firms, consistent with "
      "the Neri-Laine et al. finding and with the general literature on firm-size "
      "and regulatory compliance costs."),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 10. TECHNICAL INFRASTRUCTURE
# ════════════════════════════════════════════════════════════════════════════
story += [H("10. Technical Infrastructure", h1), HR()]

story += [
    H("10.1 Data Pipeline", h2),
    P("The data pipeline runs in four sequential steps, combining R and Stata. "
      "The pipeline runs once to build the analysis dataset; all subsequent "
      "analysis runs only from the final FST file. Original datasets are read-only; "
      "all new work goes into the <code>New/</code> directory."),
    SP(4),
    make_table([
        ["Step", "Language", "Script", "Input", "Output"],
        ["0", "Stata",
         "WB_Dataset_Conversion.do",
         "WB DTA Excel files",
         "WB_DTA.dta — World Bank DTA indices in Stata format"],
        ["1", "R",
         "1_Build_Final_PTA_EP_Dataset.R",
         "WB_DTA.dta + TREND raw data + country codes",
         "Merged_TREND_WB_Indices_Only.dta — both indices at destination-year level"],
        ["2", "Stata",
         "2_Build_Final_PTA_EP_Dataset.do",
         "Chinese customs Stata file + Merged indices",
         "final_dataset_pta_env_indices_compressed.dta (~14 GB)"],
        ["3", "R",
         "3_Build_Final_PTA_EP_Dataset.R",
         "The .dta from Step 2",
         "final_dataset_pta_env_indices_compressed.fst — fast column-selective R format"],
    ], col_widths=[1*cm, 1.5*cm, 5.5*cm, 5*cm, 4.5*cm]),
    SP(6),
    P("Steps 0 and 2 are run interactively in Stata. Steps 1 and 3 are R scripts. "
      "The FST format (R package <code>fst</code>) enables sub-second access to any "
      "column subset of the 49M-row dataset without loading the full file into memory. "
      "All analysis scripts use <code>read_fst(path, columns = needed_cols)</code>."),
    SP(6),
    H("10.2 The Shared Utility Library: pta_functions.R", h2),
    P("All analysis scripts source <code>Code/Analysis/pta_functions.R</code>, which "
      "provides the core infrastructure for model estimation, caching, and table "
      "generation. Key functions:"),
    make_table([
        ["Function", "Purpose", "How It Works"],
        ["run_block(block_name, formulas, ...)",
         "Runs a named block of fixest formulas with RDS caching.",
         "For each formula in the block, checks if the corresponding RDS file "
         "already exists in New/Output/OLS/. If so, skips; if not, calls "
         "estimate_model() and saves the result. Enables incremental computation "
         "— a crashed job can restart from the last completed model."],
        ["estimate_model(formula, fst_path, ...)",
         "Estimates a single model from the FST file, loading only required columns.",
         "Calls load_formula_data() to parse the formula and identify needed columns. "
         "Loads only those columns from FST. Runs feols() or fepois(). "
         "Returns a stats object (not the full model, to save memory)."],
        ["load_formula_data(formula_str, fst_path)",
         "Parses a fixest formula string and loads only necessary columns.",
         "Regex-parses the formula to extract variable names. Calls "
         "read_fst(fst_path, columns = needed_cols). Returns a data.table."],
        ["make_table(rds_list, ...)",
         "Generates LaTeX regression tables from stored model stats objects.",
         "Loads the list of RDS files, assembles coefficients + SEs into a "
         "matrix, and outputs a LaTeX table using the project's standard format."],
    ], col_widths=[4*cm, 5*cm, 7.5*cm]),
    SP(6),
    H("10.3 Concurrency and the OpenMP / FST Crash", h2),
    P("A critical technical constraint on Windows: running two or more R processes "
      "that <i>simultaneously</i> load the FST file causes an OpenMP allocator conflict "
      "that terminates both processes with the error "
      "<code>*** recursive gc invocation</code>. "
      "This occurs even at 4 threads per process, and is specific to the interaction "
      "between the FST package (which uses OpenMP for compression/decompression) and "
      "the fixest package (which also uses OpenMP for demeaning)."),
    P("The solution, implemented in <code>01_inference_fix.R</code> via "
      "<code>callr::r()</code>, is to serialize each estimation section as a "
      "subprocess: each section launches a fresh R process, completes, saves its "
      "RDS files, and exits — releasing all memory and OpenMP handles before the "
      "next section starts. This is slower than running sections in parallel but "
      "is the only stable approach on Windows with this data configuration."),
    P("<b>Rule:</b> Never run more than one heavy R job (anything loading the FST "
      "file with fixest models) at the same time on this machine."),
    SP(6),
    H("10.4 Thread Configuration", h2),
    P("Within a single estimation subprocess, the threading configuration is:"),
    P("threads_fst(1)                    # fst: single-thread (avoids OpenMP conflict)\n"
      "setFixest_nthreads(detectCores() - 1)  # fixest: all cores except 1",
      code_style),
    SP(4),
    P("This means FST reads are serialized (fast anyway since we load only needed columns) "
      "while fixest's alternating projections algorithm (demeaning for HDFE) uses all "
      "available CPU cores. On the 24-core machine, this gives ~23 threads to fixest, "
      "which substantially speeds up the most expensive step in estimation."),
    PageBreak()
]

# ════════════════════════════════════════════════════════════════════════════
# 11. REFERENCES
# ════════════════════════════════════════════════════════════════════════════
story += [H("11. References", h1), HR()]

refs = [
    ("Abman, R., Lundberg, C., & Ruta, M. (2024).",
     "The effectiveness of environmental provisions in regional trade agreements. "
     "<i>Journal of the European Economic Association</i>, 22(6), 2507–2548. "
     "DOI: 10.1093/jeea/jvae023"),
    ("Baccini, L. (2017).",
     "The economics and politics of preferential trade agreements. "
     "<i>Annual Review of Political Science</i>, 22, 75–92."),
    ("Bertrand, M., Duflo, E., & Mullainathan, S. (2004).",
     "How much should we trust differences-in-differences estimates? "
     "<i>Quarterly Journal of Economics</i>, 119(1), 249–275."),
    ("Brandi, C., Schwab, J., Berger, A., & Morin, J.-F. (2020).",
     "Do environmental provisions in trade agreements make exports greener? "
     "<i>World Development</i>, 129, 104899."),
    ("Callaway, B., & Sant'Anna, P. H. C. (2021).",
     "Difference-in-differences with multiple time periods. "
     "<i>Journal of Econometrics</i>, 225(2), 200–230."),
    ("Cameron, A. C., Gelbach, J. B., & Miller, D. L. (2008).",
     "Bootstrap-based improvements for inference with clustered errors. "
     "<i>Review of Economics and Statistics</i>, 90(3), 414–427."),
    ("Cherniwchan, J. (2017).",
     "Trade liberalization and the environment: Evidence from NAFTA and U.S. manufacturing. "
     "<i>Journal of International Economics</i>, 105, 130–149. "
     "DOI: 10.1016/j.jinteco.2017.01.005"),
    ("Copeland, B. R., Shapiro, J. S., & Taylor, M. S. (2022).",
     "Globalization and the environment. In G. Gopinath, E. Helpman, & K. Rogoff (Eds.), "
     "<i>Handbook of International Economics</i>, Vol. 5. Elsevier. "
     "DOI: 10.1016/bs.hesint.2022.02.002"),
    ("de Chaisemartin, C., & D'Haultfoeuille, X. (2020).",
     "Two-way fixed effects estimators with heterogeneous treatment effects. "
     "<i>American Economic Review</i>, 110(9), 2964–2996."),
    ("Dechezleprêtre, A., & Sato, M. (2017).",
     "The impacts of environmental regulations on competitiveness. "
     "<i>Review of Environmental Economics and Policy</i>, 11(2), 183–206."),
    ("Goodman-Bacon, A. (2021).",
     "Difference-in-differences with variation in treatment timing. "
     "<i>Journal of Econometrics</i>, 225(2), 254–277."),
    ("Larch, M., Shikher, S., & Yotov, Y. V. (2025).",
     "Fifteen recommendations for applied gravity research. "
     "<i>Review of International Economics</i>, forthcoming."),
    ("Low, P., & Yeats, A. (1992).",
     "Do 'dirty' industries migrate? In P. Low (Ed.), "
     "<i>International Trade and the Environment</i>. World Bank."),
    ("Mani, M., & Wheeler, D. (1998).",
     "In search of pollution havens? Dirty industry in the world economy, 1960–1995. "
     "<i>Journal of Environment and Development</i>, 7(3), 215–247."),
    ("Morin, J.-F., Dür, A., & Lechner, L. (2018).",
     "Mapping the trade and environment nexus: Insights from a new dataset. "
     "<i>Global Environmental Politics</i>, 18(1), 122–139."),
    ("Neri-Laine, A., Orefice, G., & Ruta, M. (2023).",
     "Deep trade agreements and heterogeneous firms' exports. "
     "CESifo Working Paper No. 10436."),
    ("Santos Silva, J. M. C., & Tenreyro, S. (2006).",
     "The log of gravity. <i>Review of Economics and Statistics</i>, 88(4), 641–658."),
    ("Shapiro, J. S. (2021).",
     "The environmental bias of trade policy. "
     "<i>Quarterly Journal of Economics</i>, 136(2), 831–886. "
     "DOI: 10.1093/qje/qjaa042"),
    ("Sun, L., & Abraham, S. (2021).",
     "Estimating dynamic treatment effects in event studies with heterogeneous treatment effects. "
     "<i>Journal of Econometrics</i>, 225(2), 175–199."),
]

ref_style = ParagraphStyle("Ref", parent=body_small,
    leftIndent=18, firstLineIndent=-18, spaceAfter=6)

for authors, text in refs:
    story.append(Paragraph(f"<b>{authors}</b> {text}", ref_style))

story += [
    SP(20),
    HR(),
    P("Working paper draft generated automatically from project repository state. "
      "Last updated: 10 June 2026. Repository: C:\\Work\\projects\\Paper_PTA.",
      ParagraphStyle("Footer", parent=note_style, alignment=TA_CENTER))
]

# ── Build ─────────────────────────────────────────────────────────────────────
doc.build(story)
print(f"PDF generated: {OUTPUT}")
