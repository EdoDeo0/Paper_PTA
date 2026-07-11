# -*- coding: utf-8 -*-
"""
Build: Status Report Paper_PTA — 2026-07-06
Report di stato completo del progetto, scritto per un lettore che non lo ha
mai visto: dati, diagnosi, ridisegno econometrico (con riferimenti
bibliografici per ogni scelta), risultati a oggi, bibliografia.
Output: New/Output/Status_Report_2026-07.pdf
"""

from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, PageBreak, Table, TableStyle,
    HRFlowable, KeepTogether, Image
)
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import cm
from reportlab.lib import colors
from reportlab.lib.enums import TA_CENTER, TA_JUSTIFY

OUTPUT = r"C:\Work\projects\Paper_PTA\New\Output\Status_Report_2026-07.pdf"
ES_PNG = r"C:\Work\projects\Paper_PTA\New\Output\TripleDiff\Diagnostics\eventstudy_collapsed_v2.png"

PAGE_W = A4[0] - 5 * cm

# ── Stili (coerenti con WorkingPaper_PTA_Status.pdf) ─────────────────────────
styles = getSampleStyleSheet()
C_DARK = colors.HexColor("#1a3a5c"); C_MID = colors.HexColor("#2c5f8a")
C_RED = colors.HexColor("#8b0000");  C_GREY = colors.HexColor("#555555")
C_ROW1 = colors.HexColor("#eef2f7"); C_GRID = colors.HexColor("#aaaaaa")

title_style = ParagraphStyle("T", parent=styles["Normal"], fontSize=21, leading=27,
    spaceAfter=10, alignment=TA_CENTER, fontName="Helvetica-Bold", textColor=C_DARK)
subtitle_style = ParagraphStyle("St", parent=styles["Normal"], fontSize=13, leading=18,
    spaceAfter=6, alignment=TA_CENTER, fontName="Helvetica", textColor=C_MID)
author_style = ParagraphStyle("Au", parent=styles["Normal"], fontSize=11, leading=16,
    spaceAfter=4, alignment=TA_CENTER, fontName="Helvetica-Oblique", textColor=C_DARK)
date_style = ParagraphStyle("Dt", parent=styles["Normal"], fontSize=10, leading=14,
    spaceAfter=18, alignment=TA_CENTER, fontName="Helvetica", textColor=C_GREY)
h1 = ParagraphStyle("H1", parent=styles["Normal"], fontSize=14, leading=19,
    spaceBefore=18, spaceAfter=8, fontName="Helvetica-Bold", textColor=C_DARK)
h2 = ParagraphStyle("H2", parent=styles["Normal"], fontSize=11.5, leading=16,
    spaceBefore=12, spaceAfter=5, fontName="Helvetica-Bold", textColor=C_MID)
body = ParagraphStyle("B", parent=styles["Normal"], fontSize=10, leading=15,
    spaceAfter=6, alignment=TA_JUSTIFY, fontName="Helvetica")
bullet = ParagraphStyle("Bu", parent=styles["Normal"], fontSize=10, leading=14,
    spaceAfter=3, leftIndent=18, bulletIndent=6, fontName="Helvetica")
note = ParagraphStyle("N", parent=styles["Normal"], fontSize=9, leading=13,
    spaceAfter=4, alignment=TA_JUSTIFY, fontName="Helvetica-Oblique", textColor=C_GREY)
warn = ParagraphStyle("W", parent=styles["Normal"], fontSize=9.5, leading=14,
    spaceAfter=6, leftIndent=10, fontName="Helvetica-Bold", textColor=C_RED)
bib = ParagraphStyle("Bib", parent=styles["Normal"], fontSize=9, leading=13,
    spaceAfter=5, leftIndent=16, firstLineIndent=-16, fontName="Helvetica")

_cb = ParagraphStyle("cb", parent=styles["Normal"], fontSize=8.5, leading=12,
    fontName="Helvetica")
_ch = ParagraphStyle("ch", parent=styles["Normal"], fontSize=8.5, leading=12,
    fontName="Helvetica-Bold", textColor=colors.white)

def pc(t): return Paragraph(t, _cb)
def ph(t): return Paragraph(t, _ch)

def make_table(header, rows, widths):
    data = [[ph(h) for h in header]] + [[pc(c) for c in r] for r in rows]
    t = Table(data, colWidths=widths, repeatRows=1)
    style = [
        ("BACKGROUND", (0, 0), (-1, 0), C_DARK),
        ("GRID", (0, 0), (-1, -1), 0.4, C_GRID),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("TOPPADDING", (0, 0), (-1, -1), 3),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 3),
    ]
    for i in range(1, len(rows) + 1):
        if i % 2 == 1:
            style.append(("BACKGROUND", (0, i), (-1, i), C_ROW1))
    t.setStyle(TableStyle(style))
    return t

S = []  # story

# ═════════════ FRONTESPIZIO ═════════════
S += [
    Spacer(1, 60),
    Paragraph("Environmental Provisions nei PTA cinesi<br/>e composizione dell'export", title_style),
    Spacer(1, 8),
    Paragraph("Report di stato del progetto — versione integrale per lettori esterni", subtitle_style),
    Spacer(1, 16),
    Paragraph("Edoardo Vitella", author_style),
    Paragraph("PhD Student — University of Trento &amp; Free University of Bozen", date_style),
    Paragraph("6 luglio 2026", date_style),
    HRFlowable(width="100%", thickness=1, color=C_MID),
    Spacer(1, 10),
    Paragraph(
        "<b>Sintesi.</b> Il progetto studia se le clausole ambientali (Environmental Provisions, EP) "
        "contenute negli accordi commerciali preferenziali (PTA) firmati dalla Cina tra il 2000 e il 2015 "
        "abbiano cambiato l'export cinese — e in particolare la sua <i>composizione</i> tra beni ambientali "
        "(green), beni inquinanti (dirty) e beni neutri. Dopo una prima fase che stimava l'effetto delle EP "
        "sul <i>volume</i> di export (domanda risultata non identificabile con questi dati), il progetto è "
        "stato ridisegnato attorno a una specifica triple-difference sulla composizione. I risultati a oggi: "
        "(i) l'effetto sull'export <b>green è un null preciso e stabilissimo</b> attraverso quattro disegni "
        "di stima, il wild cluster bootstrap e un test di permutazione; (ii) un iniziale segnale negativo "
        "sull'export <b>dirty non supera i test di inferenza robusta</b> (bootstrap p=0,18; dipende da un solo "
        "paese, la Corea del Sud). Il posizionamento naturale del paper è quindi un <b>«precision null»</b> "
        "sulla composizione, in dialogo diretto con Brandi et al. (2020) e Abman, Lundberg &amp; Ruta (2024).",
        body),
    PageBreak(),
]

# ═════════════ 1. DOMANDA DI RICERCA ═════════════
S += [
    Paragraph("1. La domanda di ricerca e perché è interessante", h1),
    Paragraph(
        "Quasi tutti i nuovi accordi commerciali contengono ormai capitoli ambientali: la banca dati TREND "
        "(Morin, Dür &amp; Lechner 2018) censisce circa 300 tipologie di clausole ambientali in oltre 700 PTA. "
        "La domanda se queste clausole abbiano effetti reali — sul commercio o sull'ambiente — è segnalata "
        "come questione aperta dalla rassegna di riferimento del campo (Copeland, Shapiro &amp; Taylor 2022). "
        "Due lavori recenti l'hanno affrontata: Brandi et al. (2020) con dati aggregati paese-coppia mostrano "
        "che le clausole restrittive riducono la quota di export «sporco» dei paesi in via di sviluppo e "
        "quelle liberalizzanti aumentano la quota «verde»; Abman, Lundberg &amp; Ruta (2024, JEEA) mostrano "
        "che le clausole su foreste e biodiversità annullano la deforestazione post-accordo.", body),
    Paragraph(
        "Questo progetto porta la domanda sul più grande esportatore mondiale — la Cina — usando dati "
        "doganali a livello di <b>impresa × prodotto × destinazione × anno</b>: un livello di dettaglio che "
        "né Brandi et al. (dati aggregati) né ALR (celle satellitari) possono osservare. Il vantaggio "
        "comparato del progetto è la possibilità di misurare la <b>riallocazione del paniere dentro la "
        "singola impresa</b>, nello spirito dell'evidenza within-plant di Cherniwchan (2017) su NAFTA.", body),
    Paragraph("1.1 La domanda originale e il suo problema", h2),
    Paragraph(
        "La formulazione iniziale del progetto era: «quanto export in più (o in meno) genera un punto in più "
        "di profondità ambientale dell'accordo?» — un effetto-livello dell'indice di EP depth. L'analisi "
        "diagnostica (giugno 2026) ha stabilito che <b>questa domanda non è identificabile con questi "
        "dati</b>, per tre ragioni cumulative: (1) la variazione effettiva del trattamento è di ~14 accordi "
        "(l'accordo ASEAN copre da solo 11 destinazioni con valori identici dell'indice); (2) l'indice entra "
        "in vigore insieme al PTA e quasi mai cambia dopo (solo 3 paesi hanno variazione within): «effetto "
        "delle clausole» ed «effetto dell'accordo» sono osservazionalmente la stessa cosa; (3) i risultati "
        "«significativi» delle prime stime sopravvivevano solo nelle specifiche meno sature e con clustering "
        "troppo fine — il classico sintomo di errori standard sottostimati in panel con trattamento "
        "persistente (Bertrand, Duflo &amp; Mullainathan 2004; Abadie, Athey, Imbens &amp; Wooldridge 2023).", body),
    Paragraph(
        "La prova sintetica è la «ladder» di saturazione: salendo verso strutture di effetti fissi più "
        "esigenti l'effetto si azzera monotonicamente — la firma di una selezione (la Cina firma accordi "
        "con mercati già in crescita), non di un effetto causale.", body),
    make_table(
        ["Effetti fissi", "WB depth (base)", "WB (controlli)", "TREND count (base)", "TREND (controlli)"],
        [
            ["fpd + t", "0,00143", "0,00127", "0,00055", "0,00049"],
            ["fpt + pd", "0,00439*", "0,00415**", "0,00114**", "0,00120**"],
            ["fpt + fpd", "0,00031", "0,00038", "0,00027", "0,00028"],
            ["fpd + pt", "-0,00027", "-0,00017", "0,00031", "0,00035"],
        ],
        [PAGE_W * 0.20, PAGE_W * 0.20, PAGE_W * 0.20, PAGE_W * 0.20, PAGE_W * 0.20]),
    Paragraph(
        "Tabella 1 — Ladder diagnostica (effetto-livello di EP depth su ln export; 96 modelli, cluster a "
        "destinazione). f=impresa, p=prodotto HS6, d=destinazione, t=anno. Le stelle sopravvivono solo nelle "
        "specifiche che non assorbono il livello della relazione commerciale.", note),
    Paragraph("1.2 La domanda attuale", h2),
    Paragraph(
        "Il ridisegno (giugno 2026) sposta la domanda dal volume alla <b>composizione</b>: «quando entra in "
        "vigore un PTA cinese con clausole ambientali, l'impresa cinese sposta il proprio paniere di export "
        "verso quella destinazione verso i beni green e lontano dai beni dirty, rispetto ai beni neutri?». "
        "Questa domanda è identificabile perché confronta <i>prodotti diversi dentro la stessa "
        "impresa-destinazione-anno</i>: tutto ciò che riguarda l'accordo nel suo complesso colpisce green, "
        "dirty e neutri allo stesso modo e si elide. È la stessa logica del disegno canonico "
        "settore×paese di Rajan &amp; Zingales (1998) e del contrasto «contenuto della clausola, condizionato "
        "all'avere un accordo» di Abman, Lundberg &amp; Ruta (2024).", body),
    Paragraph("1.3 Il meccanismo economico: perché (e quando) le clausole dovrebbero mordere", h2),
    Paragraph(
        "La Cina è l'<i>esportatore</i> in tutti i flussi osservati: perché una clausola ambientale in un "
        "accordo Cina–X dovrebbe cambiare l'export cinese verso X? I canali con un meccanismo commerciale "
        "esplicito sono due, e ciascuno genera una previsione falsificabile. <b>(a) Green market access</b>: "
        "se l'accordo taglia le barriere sui beni ambientali, l'export green cinese verso quel partner "
        "dovrebbe aumentare — è il canale «liberal» di Brandi et al. (2020) e l'interazione EP×green lo "
        "misura direttamente. <b>(b) Standard e non-regressione</b>: se l'accordo alza i costi di conformità "
        "sui prodotti inquinanti, l'export dirty dovrebbe diminuire — l'analogo commerciale dell'ipotesi "
        "pollution-haven (Mani &amp; Wheeler 1998; per la sintesi sull'effetto reale ma modesto delle "
        "regolazioni ambientali sulla competitività, Dechezleprêtre &amp; Sato 2017). Il resto del "
        "contenuto ambientale tipico dei PTA — cooperazione, spazio regolatorio, principi generali — non ha "
        "un canale commerciale diretto: <b>un indice che somma clausole con e senza meccanismo diluisce "
        "verso lo zero qualunque effetto reale</b>. Questo è un caveat interpretativo dichiarato del "
        "progetto: il null sull'indice aggregato è informativo sulla media delle clausole, e l'eterogeneità "
        "per sotto-indice (GreenMarketAccess, EnforcementDSM, Hard vs Soft — già costruiti) è la naturale "
        "estensione in agenda.", body),
]

# ═════════════ 2. DATI ═════════════
S += [
    PageBreak(),
    Paragraph("2. I dati", h1),
    make_table(
        ["Fonte", "Contenuto", "Ruolo nel progetto"],
        [
            ["Dogane cinesi 2000–2015 (non pubbliche)",
             "49,2 mln di righe: impresa × HS6 × destinazione × anno; ~462.000 imprese; valore, quantità, unit value",
             "Outcome (ln export) e dimensioni del panel; file canonico Windows: 49.245.304 righe (MD5 registrato)"],
            ["World Bank Deep Trade Agreements (Hofmann, Osnago &amp; Ruta 2017)",
             "Codifica clausola-per-clausola dei PTA; foglio Environmental Laws",
             "Indice WB_EP_Depth (somma clausole ambientali) + TotalDepth non ambientale (controllo)"],
            ["TREND (Morin, Dür &amp; Lechner 2018)",
             "~300 tipologie di clausole ambientali in 775 PTA",
             "Indice alternativo TREND_EP_Count e sotto-indici (enforcement, green market access...)"],
            ["OECD Combined List of Environmental Goods (CLEG)",
             "247 codici HS6 di beni ambientali",
             "Dummy green; lista tradotta a vintage HS1996 (match univoco 247/247)"],
            ["Settori dirty alla Mani &amp; Wheeler (1998) / Low &amp; Yeats (1992)",
             "Pulp&amp;paper, chimica di base, raffinazione, siderurgia, metalli non ferrosi (+cemento)",
             "Dummy dirty: 1.139 HS6 via concordanza ufficiale WITS HS1996↔ISIC Rev.3"],
        ],
        [PAGE_W * 0.28, PAGE_W * 0.37, PAGE_W * 0.35]),
    Spacer(1, 6),
    Paragraph("2.1 Numeri descrittivi essenziali", h2),
    Paragraph(
        "Il panel di lavoro (esclusi Hong Kong e Macao, v. sotto) conta <b>45,78 milioni di osservazioni</b> "
        "impresa-prodotto-destinazione-anno su 2000–2015, ~462.000 imprese esportatrici distinte, ~5.000 "
        "prodotti HS6 e 236 destinazioni. I beni <b>green</b> coprono l'11,5% delle osservazioni, i "
        "<b>dirty</b> il 7,0%; il resto sono i beni «neutri» che fungono da riferimento nella "
        "triple-difference. Circa il 20% delle osservazioni riguarda destinazioni con un PTA attivo con "
        "clausole ambientali.", body),
    Paragraph("2.2 Il trattamento", h2),
    Paragraph(
        "25 destinazioni trattate, ~14 accordi effettivi, entrata in vigore scaglionata 2002–2015. L'indice "
        "di profondità è affiancato dal controllo TotalDepth (profondità non ambientale dell'accordo), senza "
        "il quale l'interazione confonderebbe «clausole verdi» con «accordo profondo in generale» — lo "
        "stesso accorgimento di Brandi et al. (2020), che controllano per l'indice DESTA di Dür, Baccini "
        "&amp; Elsig (2014).", body),
    make_table(
        ["Accordo (partner)", "In vigore", "Note per l'identificazione"],
        [
            ["Bangkok Agreement / APTA (Bangladesh, India, Corea, Laos, Sri Lanka)", "2002 / 2005",
             "Primo accordo del periodo; contenuto ambientale minimo"],
            ["ASEAN–Cina (11 destinazioni)", "2005",
             "Un solo accordo contato 11 volte: valori dell'indice identici per tutti i membri — "
             "è il motivo per cui i «25 paesi trattati» sono ~14 accordi effettivi"],
            ["Cile", "2006", "—"],
            ["Pakistan", "2007", "—"],
            ["Nuova Zelanda", "2008", "Primo accordo cinese con capitolo ambientale sostanziale"],
            ["Singapore", "2009", "Anche membro ASEAN: prevale il valore massimo tra accordi"],
            ["Perù", "2010", "—"],
            ["Costa Rica", "2011", "—"],
            ["Islanda; Svizzera", "2014", "Accordi con partner avanzati, contenuto ambientale più ricco"],
            ["Australia; Corea del Sud", "2015",
             "Ultimo anno del panel: contribuiscono solo al margine; la Corea è uno dei 3 soli paesi con "
             "variazione within dell'indice (APTA 2002 → FTA 2015)"],
            ["Hong Kong; Macao (CEPA)", "2003",
             "ESCLUSI dalla specifica principale: entrepôt (re-export), accordo sui generis, e da soli il "
             "50% del valore export «trattato» — li si reintroduce solo come robustezza"],
        ],
        [PAGE_W * 0.38, PAGE_W * 0.14, PAGE_W * 0.48]),
    Paragraph(
        "Tabella — Mappa del trattamento. La colonna delle note anticipa i tre fatti che disciplinano tutta "
        "l'inferenza: pochi accordi effettivi, quasi nessuna variazione within-paese, un blocco (ASEAN) che "
        "domina il conteggio dei trattati.", note),
    Paragraph("2.3 Un problema dati risolto: la vintage dei codici prodotto", h2),
    Paragraph(
        "I codici HS6 cambiano con le revisioni 2002/2007/2012 della nomenclatura: se pannello e liste "
        "green/dirty usano vintage diverse, le dummy sono male assegnate e gli effetti fissi spezzano le "
        "serie. L'audit ha accertato che il pannello è in HS1996 mentre la lista OCSE era nativa HS2012: la "
        "lista è stata tradotta a HS1996 (247/247 codici con corrispondenza univoca, zero perdite di valore "
        "ai confini di revisione), la lista dirty è stata costruita direttamente in HS1996 dalla concordanza "
        "ufficiale WITS/UNSD, e 17 codici presenti in entrambe le liste (es. materiali isolanti: prodotti "
        "da industrie inquinanti ma a uso ambientale) sono stati assegnati al green, che è una lista curata "
        "prodotto-per-prodotto: le due categorie sono ora mutuamente esclusive.", body),
]

# ═════════════ 3. SPECIFICA ═════════════
S += [
    Paragraph("3. La specifica principale e le scelte econometriche", h1),
    Paragraph(
        "<b>ln export<sub>fpdt</sub> = β<sub>1</sub>·EP<sub>dt</sub>×green<sub>p</sub> + "
        "β<sub>2</sub>·EP<sub>dt</sub>×dirty<sub>p</sub> + γ<sub>1</sub>·TotalDepth<sub>dt</sub>×green<sub>p</sub> "
        "+ γ<sub>2</sub>·TotalDepth<sub>dt</sub>×dirty<sub>p</sub> + θ<sub>fpd</sub> + θ<sub>fdt</sub> + "
        "θ<sub>pt</sub> + ε</b>, cluster a destinazione.", body),
    Paragraph("Ogni pezzo della specifica risponde a una minaccia identificativa precisa:", body),
    make_table(
        ["Scelta", "Minaccia a cui risponde", "Riferimento"],
        [
            ["FE impresa×dest×anno (fdt)",
             "Assorbe TUTTO ciò che varia a livello di relazione impresa-mercato nel tempo, incluso il PTA "
             "stesso, la crescita del mercato e la selezione negli accordi: il confondente principale "
             "sparisce per costruzione",
             "Logica del disegno interazione Rajan &amp; Zingales (1998); contrasto within-accordo di ALR (2024)"],
            ["FE prodotto×anno (pt)",
             "Shock globali di prodotto (es. boom mondiale del fotovoltaico) che colpirebbero i green "
             "ovunque", "Prassi gravity strutturale (Head &amp; Mayer 2014)"],
            ["FE impresa×prodotto×dest (fpd)",
             "Livello della relazione (specializzazione storica dell'impresa in quel prodotto verso quel "
             "mercato)", "—"],
            ["TotalDepth×green/dirty",
             "Distinguere il contenuto ambientale dalla profondità complessiva dell'accordo (correlate)",
             "Brandi et al. (2020): controllo DESTA depth"],
            ["Cluster a destinazione (236 gruppi)",
             "Il trattamento varia a livello destinazione×anno: clusterizzare più fine sottostima gli SE",
             "Abadie, Athey, Imbens &amp; Wooldridge (2023); Bertrand, Duflo &amp; Mullainathan (2004)"],
            ["Outcome in log su flussi positivi (OLS)",
             "Margine intensivo; il margine estensivo richiede PPML con zeri (in agenda su sub-campioni)",
             "Santos Silva &amp; Tenreyro (2006); raccomandazioni di Larch, Shikher &amp; Yotov (2025)"],
        ],
        [PAGE_W * 0.26, PAGE_W * 0.46, PAGE_W * 0.28]),
    Spacer(1, 6),
    Paragraph("3.1 Inferenza con pochi cluster trattati", h2),
    Paragraph(
        "I cluster totali sono 236, ma i <i>trattati</i> sono ~23 e gli accordi effettivi ~14: gli errori "
        "standard asintotici sono inaffidabili in questo regime. Il progetto adotta quindi un'inferenza a "
        "tre livelli: (i) cluster-robust asintotici come riferimento; (ii) <b>wild cluster bootstrap</b> "
        "(Cameron, Gelbach &amp; Miller 2008; implementazione Fischer &amp; Roodman 2021), il rimedio "
        "standard per pochi cluster; (iii) <b>test di permutazione</b> alla Fisher (1935), nella variante "
        "usata da Bertrand et al. (2004): i profili ambientali completi (profondità e timing) vengono "
        "riassegnati casualmente 1.000 volte tra le destinazioni trattate, e si verifica se il coefficiente "
        "osservato spicca nella distribuzione dei placebo. Questo terzo test risponde alla domanda più "
        "insidiosa: «è davvero il contenuto ambientale, o qualsiasi etichetta assegnata a quei paesi darebbe "
        "lo stesso numero?».", body),
    Paragraph("3.2 Stimatori e robustezza dinamica", h2),
    Paragraph(
        "Il timing scaglionato degli accordi espone il TWFE classico ai problemi di pesi negativi e "
        "contaminazione tra coorti (Goodman-Bacon 2021; de Chaisemartin &amp; D'Haultfœuille 2020). Le "
        "difese in campo: event study differenziale con pre-trend osservabili; bin accumulati agli estremi "
        "dichiarati; e in agenda la stima Sun &amp; Abraham (2021) / Callaway &amp; Sant'Anna (2021) sui "
        "sub-campioni computazionalmente trattabili. La deriva negativa del green a +5 anni (v. figura) è "
        "identificata solo dalle coorti precoci (ASEAN 2005, Cile 2006, Pakistan 2007, NZ 2008) e non va "
        "interpretata prima della verifica Sun-Abraham.", body),
]

# ═════════════ 4. GRUPPI DI CONTROLLO ═════════════
S += [
    PageBreak(),
    Paragraph("4. Gruppi di controllo e strategia computazionale", h1),
    Paragraph("4.1 Sub-campioni (Fase R-control)", h2),
    Paragraph(
        "Sul modello dei gruppi di controllo multipli di Caselli, Huang, Tomasi &amp; Zhu (w.p. sul dataset "
        "gemello, anti-dumping e qualità), la triple-diff viene ristimata su sub-campioni che restringono il "
        "gruppo di confronto. La logica generale: restringere su caratteristiche <i>pre-trattamento</i> "
        "produce un effetto condizionato valido (nessun bias di selezione indotto dal restringimento), e "
        "<b>la stabilità del coefficiente attraverso i gruppi è essa stessa il test</b> — un effetto vero "
        "non dovrebbe dipendere da chi funge da controllo.", body),
    make_table(
        ["Gruppo", "Costruzione e numeri", "Pro / contro econometrici"],
        [
            ["C-prod-HS4",
             "Solo i non-green nella stessa famiglia HS4 di un green: 106 famiglie, 353 prodotti di "
             "controllo, 9,5 mln di righe (20,5%)",
             "Trend di prodotto più simili; esposto agli spillover within-firm tra prodotti della stessa "
             "impresa multiprodotto (Eckel &amp; Neary 2010) → mai riportato da solo"],
            ["C-prod-match",
             "Matching CEM (Iacus, King &amp; Porro 2012) dei non-green sui green su covariate pre-periodo "
             "(valore, unit value, concentrazione HHI) entro capitolo HS2: 228 green matchati",
             "Bilanciamento verificato su love plot: 2 covariate su 3 sotto soglia SMD 0,1; la "
             "concentrazione resta a ~0,18 → limite dichiarato"],
            ["C-overlap",
             "Solo HS6 esportati sia verso trattati sia verso controlli (common support): 98,5% degli HS6, "
             "~100% delle righe",
             "Il più pulito (niente estrapolazione) ma taglia pochissimo → computazionalmente equivalente "
             "al full panel"],
            ["C-deepshallow",
             "Solo partner PTA, confronto deep vs shallow EP (17 vs 8 paesi, mediana della depth massima): "
             "11,8 mln di righe",
             "Elimina per costruzione la selezione trattati/mai-trattati (la variante à la ALR 2024); "
             "gli 8 cluster shallow rendono l'inferenza ancora più delicata"],
            ["Paesi CEM (v1)",
             "16 trattati + 40 controlli matchati su PIL pro capite, crescita, tariffa MFN al 2000; "
             "27,8 mln di righe",
             "Bilanciamento verificato; una variante «v2» con baseline commerciale pre-PTA è stata "
             "testata e scartata (perde metà dei trattati senza bilanciare la covariata aggiunta)"],
        ],
        [PAGE_W * 0.16, PAGE_W * 0.44, PAGE_W * 0.40]),
    Spacer(1, 6),
    Paragraph("4.2 Il vincolo computazionale e il panel collassato", h2),
    Paragraph(
        "La stima full-panel (45,8 mln di righe, tre livelli di FE ad alta dimensionalità: ~decine di "
        "milioni di gruppi impresa-prodotto-destinazione e impresa-destinazione-anno) eccede le capacità "
        "della workstation: l'allocatore di R/fixest (Bergé 2018) fallisce sistematicamente, in qualsiasi "
        "configurazione provata (processo diretto o sottoprocesso, 4–12 thread, con pulizia memoria "
        "aggressiva), pur con 61,6 GB di RAM di cui ~50 liberi — un limite dell'infrastruttura software, "
        "non della memoria fisica. La risposta strutturale — non un ripiego — è che <b>la domanda di "
        "composizione non richiede il livello impresa</b>: il panel viene collassato a cella "
        "prodotto×destinazione×anno (3,77 mln di celle, outcome = media di ln export, pesi = numero di "
        "osservazioni), con FE pd+dt+pt che replicano uno-a-uno la logica identificativa (dt assorbe il "
        "PTA come fdt nel full panel). L'outcome in media-di-log (non log-di-somma) evita distorsioni da "
        "disuguaglianza di Jensen nel confronto col full panel. Ciò che si perde nel collasso è solo la "
        "variazione within-firm — che resta riservata al modulo dedicato (Fase R4), dove il livello "
        "impresa serve davvero.", body),
    Paragraph(
        "Il full panel è stato infine stimato con reghdfe/Stata (Correia 2017), algoritmicamente più "
        "parsimonioso e con rimozione iterativa dei singleton (24,3 milioni rimossi → 21,5 milioni di "
        "osservazioni effettive, convergenza in 89 iterazioni): i risultati (v. §5) replicano il panel "
        "collassato, e i merge riproducono al centesimo le quote green/dirty della pipeline R — un "
        "controllo di coerenza incrociata tra due implementazioni indipendenti.", body),
    Paragraph(
        "Nota di metodo sui tentativi falliti: anche il wild bootstrap ha richiesto un adattamento — "
        "l'oggetto di stima completo necessario a boottest non è costruibile su questa macchina, quindi il "
        "WCB è stato eseguito sul modello equivalente per partialling-out di Frisch–Waugh (demeaning pesato "
        "rispetto alle tre FE, poi regressione semplice: coefficienti identici, verificati contro la stima "
        "diretta). Ogni deviazione di questo tipo è documentata nello script corrispondente.", body),
]

# ═════════════ 5. RISULTATI ═════════════
S += [
    Paragraph("5. I risultati a oggi", h1),
    Paragraph("5.1 EP × green: un null preciso e stabile", h2),
    make_table(
        ["Disegno", "Campione", "EP(WB)×green", "p asint.", "p WCB (B=9999)"],
        [
            ["FULL PANEL firm-level (reghdfe)", "21,5 mln oss. eff.", "-0,0021", "0,55", "—"],
            ["Collassato (analogo full)", "3,68 mln celle", "-0,0023", "0,72", "0,88"],
            ["C-prod-HS4 (firm-level)", "3,77 mln oss. eff.", "-0,0009", "0,84", "—"],
            ["Paesi CEM (firm-level)", "13,7 mln oss. eff.", "-0,0022", "0,49", "—"],
            ["C-deepshallow (firm-level)", "solo partner PTA", "-0,0021", "0,50", "—"],
        ],
        [PAGE_W * 0.26, PAGE_W * 0.22, PAGE_W * 0.18, PAGE_W * 0.14, PAGE_W * 0.20]),
    Paragraph(
        "Tabella 2 — Stabilità dell'interazione EP×green (indice WB; il TREND dà lo stesso quadro: sul "
        "full panel -0,0001 con p=0,91, sul collassato +0,0018 con p WCB 0,39). Test di permutazione sul "
        "collassato: p=0,45. Il coefficiente è quasi identico in cinque disegni con campioni e gruppi di "
        "controllo diversi, sempre indistinguibile da zero: un null stimato con precisione, non assenza di "
        "potenza. La riga full panel è la specifica principale §3 stimata via reghdfe (24,3 mln di "
        "singleton rimossi iterativamente; 225 cluster); i test congiunti sulle 4 interazioni: "
        "F(4;224)=1,32, p=0,26 (WB) e F(4;224)=0,53, p=0,71 (TREND) — la composizione è congiuntamente "
        "nulla anche con SE asintotici.", note),
    Paragraph("5.2 EP × dirty: una pista aperta e chiusa dall'inferenza robusta", h2),
    Paragraph(
        "Il livello prodotto aveva prodotto un candidato risultato in direzione Brandi: EP(WB)×dirty = "
        "-0,0089 con p asintotico 0,006 sul collassato, -0,0040 (p=0,056) sui paesi CEM. Tre test "
        "indipendenti lo smontano: (i) il wild cluster bootstrap porta il p a <b>0,18</b>; (ii) il test di "
        "permutazione a livello aggregato inverte il segno (+0,004, p=0,50); (iii) il leave-one-out sui 23 "
        "paesi trattati mostra che <b>senza la sola Corea del Sud</b> il coefficiente scende a -0,0059 con "
        "p=0,21 — e la Corea è uno dei tre soli paesi con variazione within dell'indice (il salto del FTA "
        "2015), quindi porta una quota sproporzionata dell'identificazione. L'indice TREND non ha mai "
        "confermato (p WCB 0,85). Il full panel (reghdfe) è coerente con questo quadro: -0,0040 con p "
        "asintotico 0,038 — la stessa grandezza del campione CEM, con un p che i test robusti sui pochi "
        "cluster trattati (come visto sopra) portano oltre le soglie convenzionali. Conclusione: <b>pista "
        "chiusa</b> — nessun effetto robusto nemmeno sul margine dirty.", body),
    make_table(
        ["Interazione (collassato)", "Coeff.", "p asintotico", "p WCB", "Permutation"],
        [
            ["WB × green", "-0,0023", "0,72", "0,88", "0,45"],
            ["WB × dirty", "-0,0089", "0,006", "0,18", "0,50 (segno +)"],
            ["TREND × green", "+0,0018", "0,32", "0,39", "—"],
            ["TREND × dirty", "+0,0004", "0,83", "0,85", "—"],
        ],
        [PAGE_W * 0.28, PAGE_W * 0.14, PAGE_W * 0.20, PAGE_W * 0.14, PAGE_W * 0.24]),
    Paragraph(
        "Tabella 3 — Inferenza a tre livelli sul panel collassato. Il contrasto tra il p asintotico e il p "
        "bootstrap sulla riga WB×dirty è un caso da manuale del problema dei pochi cluster (CGM 2008).", note),
    Spacer(1, 4),
    make_table(
        ["Esercizio leave-one-out (WB × dirty, collassato)", "Coeff.", "p asintotico"],
        [
            ["Baseline (tutti i trattati)", "-0,0089", "0,006"],
            ["Escludendo la Corea del Sud", "-0,0059", "0,210"],
            ["Escludendo uno qualsiasi degli altri paesi (intervallo su 10 stime)", "-0,0088 … -0,0099", "0,0005 … 0,0065"],
        ],
        [PAGE_W * 0.56, PAGE_W * 0.20, PAGE_W * 0.24]),
    Paragraph(
        "Tabella 4 — Fragilità del candidato risultato dirty: l'esclusione di un singolo paese (la Corea, "
        "uno dei tre soli con variazione within dell'indice) elimina la significatività. Un coefficiente che "
        "vive o muore con un paese non è un risultato pubblicabile — è al più un'ipotesi per il full panel.", note),
    Spacer(1, 4),
]
S.append(Image(ES_PNG, width=PAGE_W, height=PAGE_W * 960 / 2200))
S += [
    Paragraph(
        "Figura 1 — Event study differenziale (panel collassato): effetto su green e dirty rispetto ai "
        "neutri attorno all'entrata in vigore. Pre-trend piatti (validazione del disegno), nessun salto a "
        "t=0 (il null), deriva verde tardiva identificata dalle sole coorti precoci (caveat Sun-Abraham).", note),
    Paragraph("5.3 Che paper è, quindi", h2),
    Paragraph(
        "Il quadro converge verso un <b>precision null sulla composizione</b>: con 46 milioni di "
        "osservazioni, liste green/dirty curate e quattro disegni di controllo, le clausole ambientali dei "
        "PTA cinesi non spostano né l'export verde né quello sporco. Il posizionamento: (i) contro Brandi "
        "et al. (2020), che trovano effetti di composizione per i paesi in via di sviluppo — la Cina, "
        "l'esportatore dominante, non risponde; (ii) contro ALR (2024), che trovano effetti reali delle "
        "clausole specifiche — su outcome ambientali, non commerciali; (iii) dentro la cornice di Copeland, "
        "Shapiro &amp; Taylor (2022), che indicano il contenuto ambientale degli accordi come questione "
        "aperta. Un'interpretazione economica coerente: quasi tutte le clausole nei PTA cinesi del periodo "
        "sono di cooperazione soft, senza il meccanismo commerciale (taglio tariffario sui green, standard "
        "vincolanti sui dirty) che muoverebbe i flussi — coerente anche con l'evidenza che le politiche "
        "commerciali implicite favoriscono i settori sporchi (Shapiro 2021).", body),
]

# ═════════════ 6. STATO E PROSSIMI PASSI ═════════════
S += [
    PageBreak(),
    Paragraph("6. Stato di avanzamento e prossimi passi", h1),
    make_table(
        ["Blocco", "Stato", "Note"],
        [
            ["Igiene dati (audit R1: vintage HS, trattamento, HK+MO, imprese)", "Chiuso",
             "Include audit indipendente del codice (4 bug critici trovati e corretti, 2026-07-03)"],
            ["Liste green (HS1996) e dirty (WITS/ISIC3)", "Chiuso", "Mutuamente esclusive; 247 + 1.139 HS6"],
            ["TotalDepth non ambientale", "Chiuso", "Validato contro l'indice EP"],
            ["Triple-diff su sub-campioni + collassato", "Chiuso", "Tabelle di stabilità prodotte"],
            ["Inferenza (WCB + permutation + leave-one-out)", "Chiuso", "Pista dirty chiusa"],
            ["Event study", "Chiuso (v. caveat)", "Verifica Sun-Abraham in agenda"],
            ["Conferma full-panel", "Chiusa (WB e TREND)",
             "reghdfe/Stata riesce dove R/fixest non era fattibile: WB×green -0,0021 (p=0,55, F congiunto "
             "p=0,26); TREND×green -0,0001 (p=0,91, F congiunto p=0,71) — precision null confermato"],
            ["Tariffe preferenziali WITS", "Bloccato (esterno)", "API SDMX della Banca Mondiale fuori servizio; MFN con caveat nel frattempo"],
            ["PPML con zeri (margine estensivo) su sub-campioni", "Da fare", "Fase R4; Santos Silva &amp; Tenreyro (2006)"],
            ["Riallocazione within-firm (quota green nel paniere)", "Da fare", "Fase R4 — il potenziale contributo distintivo"],
            ["Scrittura e framing definitivo", "Da fare", "Dopo conferma full-panel"],
        ],
        [PAGE_W * 0.44, PAGE_W * 0.18, PAGE_W * 0.38]),
    Spacer(1, 8),
    Paragraph("6.1 Il bivio di framing", h2),
    Paragraph(
        "La decisione finale sul posizionamento del paper dipende da due esiti pendenti, entrambi a basso "
        "rischio di ribaltamento. <b>Scenario A (atteso)</b>: la conferma full-panel via reghdfe riproduce i "
        "null — il paper è un precision null sulla composizione dell'export del più grande esportatore "
        "mondiale, con la stabilità attraverso i gruppi di controllo come contributo metodologico e il "
        "contrasto con Brandi et al. (2020) e ALR (2024) come contributo sostanziale; target naturale: "
        "riviste di sviluppo/ambiente (World Development, JEEM). <b>Scenario B (improbabile)</b>: il full "
        "panel firm-level fa emergere un'interazione robusta che il collassato non vede (es. concentrata "
        "nelle imprese multiprodotto) — in quel caso il modulo within-firm diventa il cuore del paper e il "
        "target sale (JIE/JEEM). In entrambi gli scenari, l'eterogeneità per sotto-indice (§1.3) e il "
        "margine estensivo via PPML restano le estensioni con il miglior rapporto valore/costo.", body),
    Paragraph(
        "Nota di trasparenza: tutti i numeri di questo report provengono da script versionati nella cartella "
        "di lavoro del progetto (New/Code/01–16) con output riproducibili su file; il dataset canonico è "
        "fissato con hash MD5. Nessun file originale della pipeline è stato modificato. Il progetto è stato "
        "inoltre sottoposto a un audit indipendente del codice (2026-07-03) che ha identificato e corretto "
        "quattro errori critici prima di qualsiasi stima definitiva — tra cui una variabile con nome "
        "fuorviante (ln_export_value è in realtà il log dello unit value) che aveva inquinato due "
        "diagnostiche di matching.", note),
]

# ═════════════ APPENDICE: SCRIPT ═════════════
S += [
    PageBreak(),
    Paragraph("Appendice A — Inventario degli script di analisi (New/Code/)", h1),
    Paragraph(
        "Tutti gli script sono in R salvo indicazione; ognuno scrive output riproducibili su file "
        "(diagnostiche .md/.txt/.csv, modelli .rds con cache, grafici .png). L'ordine numerico riflette "
        "la sequenza logica della pipeline.", body),
    make_table(
        ["Script", "Cosa fa"],
        [
            ["01 (+01c/01d)", "Fase 1: ri-stima delle 4 strutture FE con clustering uniforme a destinazione; ladder diagnostica (Tabella 1)"],
            ["02, 02b", "Audit igiene dati: stabilità HS6 ai confini di revisione, mappa del trattamento, peso HK+MO, outlier unit value, consistenza imprese"],
            ["03, 03b, 03c", "La vicenda vintage HS: tentativo di concordanza completa (abbandonato e documentato), traduzione della lista green a HS1996 (247/247 univoci), verifica di continuità sui codici corretti"],
            ["04", "Download tariffe preferenziali da WITS (API SDMX) — sospeso: API fuori servizio lato server, documentato"],
            ["05", "Classificazione dirty: settori Mani-Wheeler ISIC2 → ISIC3 → HS6 via concordanza ufficiale WITS; risoluzione overlap col green"],
            ["06", "TotalDepth non ambientale per destinazione-anno dai dati WB, con validazione interna"],
            ["07, 07b", "Triple-diff full-panel (3 sezioni: stime, event study, permutation) — non eseguibile sulla workstation, v. §4.2; 07b è il retry documentato in sessione diretta"],
            ["08–11", "Costruzione dei 4 sub-campioni di controllo (flag riusabili su file) con diagnostiche di bilanciamento"],
            ["12", "CEM v2 (baseline commerciale come covariata aggiuntiva): testato e scartato con motivazione"],
            ["13", "Triple-diff sui sub-campioni → tabella di stabilità (Tabella 2)"],
            ["14, 14b, 14c", "Panel collassato: stime principali, event study, permutation green e dirty, grafico (Figura 1)"],
            ["15, 15b", "Wild cluster bootstrap (via Frisch-Waugh) e leave-one-out sul candidato dirty (Tabelle 3-4)"],
            ["16 (Stata)", "Conferma full-panel via reghdfe — in esecuzione"],
        ],
        [PAGE_W * 0.18, PAGE_W * 0.82]),
]

# ═════════════ BIBLIOGRAFIA ═════════════
REFS = [
    "Abadie, A., Athey, S., Imbens, G. W., &amp; Wooldridge, J. M. (2023). When should you adjust standard "
    "errors for clustering? <i>Quarterly Journal of Economics</i>, 138(1), 1–35.",
    "Abman, R., Lundberg, C., &amp; Ruta, M. (2024). The effectiveness of environmental provisions in "
    "regional trade agreements. <i>Journal of the European Economic Association</i>, 22(6), 2507–2548.",
    "Baccini, L., Pinto, P. M., &amp; Weymouth, S. (2017). The distributional consequences of preferential "
    "trade liberalization: firm-level evidence. <i>International Organization</i>, 71(2), 373–395.",
    "Bergé, L. (2018). Efficient estimation of maximum likelihood models with multiple fixed-effects: the "
    "R package FENmlm. <i>CREA Discussion Papers</i>, 13.",
    "Bertrand, M., Duflo, E., &amp; Mullainathan, S. (2004). How much should we trust "
    "differences-in-differences estimates? <i>Quarterly Journal of Economics</i>, 119(1), 249–275.",
    "Brandi, C., Schwab, J., Berger, A., &amp; Morin, J.-F. (2020). Do environmental provisions in trade "
    "agreements make exports from developing countries greener? <i>World Development</i>, 129, 104899.",
    "Callaway, B., &amp; Sant'Anna, P. H. C. (2021). Difference-in-differences with multiple time periods. "
    "<i>Journal of Econometrics</i>, 225(2), 200–230.",
    "Cameron, A. C., Gelbach, J. B., &amp; Miller, D. L. (2008). Bootstrap-based improvements for inference "
    "with clustered errors. <i>Review of Economics and Statistics</i>, 90(3), 414–427.",
    "Caselli, M., Huang, S., Tomasi, C., &amp; Zhu, M. (working paper). Anti-dumping and product quality. "
    "[disegno dei gruppi di controllo multipli sul dataset doganale cinese]",
    "Cherniwchan, J. (2017). Trade liberalization and the environment: evidence from NAFTA and U.S. "
    "manufacturing. <i>Journal of International Economics</i>, 105, 130–149.",
    "Copeland, B. R., Shapiro, J. S., &amp; Taylor, M. S. (2022). Globalization and the environment. In "
    "<i>Handbook of International Economics</i> (Vol. 5, pp. 61–146). Elsevier.",
    "Correia, S. (2017). Linear models with high-dimensional fixed effects: an efficient and feasible "
    "estimator. Working paper. [implementazione reghdfe]",
    "de Chaisemartin, C., &amp; D'Haultfœuille, X. (2020). Two-way fixed effects estimators with "
    "heterogeneous treatment effects. <i>American Economic Review</i>, 110(9), 2964–2996.",
    "Dechezleprêtre, A., &amp; Sato, M. (2017). The impacts of environmental regulations on competitiveness. "
    "<i>Review of Environmental Economics and Policy</i>, 11(2), 183–206.",
    "Dür, A., Baccini, L., &amp; Elsig, M. (2014). The design of international trade agreements: introducing "
    "a new dataset. <i>Review of International Organizations</i>, 9(3), 353–375.",
    "Eckel, C., &amp; Neary, J. P. (2010). Multi-product firms and flexible manufacturing in the global "
    "economy. <i>Review of Economic Studies</i>, 77(1), 188–217.",
    "Fischer, A., &amp; Roodman, D. (2021). fwildclusterboot: fast wild cluster bootstrap inference for "
    "linear regression models. R package.",
    "Fisher, R. A. (1935). <i>The Design of Experiments</i>. Oliver &amp; Boyd. [inferenza per permutazione]",
    "Goodman-Bacon, A. (2021). Difference-in-differences with variation in treatment timing. <i>Journal of "
    "Econometrics</i>, 225(2), 254–277.",
    "Head, K., &amp; Mayer, T. (2014). Gravity equations: workhorse, toolkit, and cookbook. In <i>Handbook "
    "of International Economics</i> (Vol. 4, pp. 131–195). Elsevier.",
    "Hofmann, C., Osnago, A., &amp; Ruta, M. (2017). Horizontal depth: a new database on the content of "
    "preferential trade agreements. <i>World Bank Policy Research Working Paper</i>, 7981.",
    "Iacus, S. M., King, G., &amp; Porro, G. (2012). Causal inference without balance checking: coarsened "
    "exact matching. <i>Political Analysis</i>, 20(1), 1–24.",
    "Larch, M., Shikher, S., &amp; Yotov, Y. V. (2025). Recommendations for gravity estimations. "
    "<i>Review of International Economics</i>.",
    "Low, P., &amp; Yeats, A. (1992). Do 'dirty' industries migrate? In P. Low (ed.), <i>International "
    "Trade and the Environment</i>, World Bank Discussion Paper 159.",
    "Mani, M., &amp; Wheeler, D. (1998). In search of pollution havens? Dirty industry in the world "
    "economy, 1960–1995. <i>Journal of Environment &amp; Development</i>, 7(3), 215–247.",
    "Morin, J.-F., Dür, A., &amp; Lechner, L. (2018). Mapping the trade and environment nexus: insights "
    "from a new dataset. <i>Global Environmental Politics</i>, 18(1), 122–139.",
    "Neri-Lainé, M., Orefice, G., &amp; Ruta, M. (2023). Deep trade agreements and firm exports. "
    "<i>CESifo Working Paper</i>, 10436.",
    "Rajan, R. G., &amp; Zingales, L. (1998). Financial dependence and growth. <i>American Economic "
    "Review</i>, 88(3), 559–586.",
    "Santos Silva, J. M. C., &amp; Tenreyro, S. (2006). The log of gravity. <i>Review of Economics and "
    "Statistics</i>, 88(4), 641–658.",
    "Shapiro, J. S. (2021). The environmental bias of trade policy. <i>Quarterly Journal of Economics</i>, "
    "136(2), 831–886.",
    "Sun, L., &amp; Abraham, S. (2021). Estimating dynamic treatment effects in event studies with "
    "heterogeneous treatment effects. <i>Journal of Econometrics</i>, 225(2), 175–199.",
]
S += [PageBreak(), Paragraph("Bibliografia", h1)]
S += [Paragraph(r, bib) for r in REFS]

doc = SimpleDocTemplate(OUTPUT, pagesize=A4,
                        leftMargin=2.5 * cm, rightMargin=2.5 * cm,
                        topMargin=2.2 * cm, bottomMargin=2.2 * cm,
                        title="Status Report Paper_PTA — luglio 2026",
                        author="Edoardo Vitella")
doc.build(S)
print("[OK]", OUTPUT)
