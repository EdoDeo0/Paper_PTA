# World Bank Deep Trade Agreements (DTA) Database - Codebook

## Version Information

**Dataset:** DTA 1.0 - Horizontal Content (v2)
**Updated:** January 23, 2024
**Data Format:** Excel spreadsheet with multiple sheets

---

## Overview

The World Bank's Deep Trade Agreements (DTA) Database provides comprehensive information on preferential trade agreements (PTAs) and their content provisions. The dataset is organized horizontally, with one row per agreement and multiple columns representing different provisions and agreement characteristics.

The database includes:
- 397+ preferential trade agreements
- Historical bilateral trade relationships (1970-2023)
- Detailed content analysis of WTO-covered and WTO-X provisions
- Agreement specifications and legal enforceability information

---

## Dataset Structure

The dataset is distributed across multiple sheets:

### Sheet 1: WTO AC (WTO Covered Areas - Agreements Commitment)
Contains binary (0/1) indicators for WTO-mandated provisions that are mentioned in agreements.

**Variables:**
- RTAID: Unique identifier from WTO Regional Trade Agreements Database
- WBID: Unique identifier from WB Deep Trade Agreements Database
- Agreement: Name of the trade agreement
- FTA Industrial: Tariff liberalization on industrial goods, elimination of non-tariff measures
- FTA Agriculture: Tariff liberalization on agricultural goods, elimination of non-tariff measures
- Customs: Information provision, publication on Internet, training
- Export Taxes: Elimination of export taxes
- SPS: Sanitary and Phytosanitary measures - affirmation of WTO rights and obligations, harmonization
- TBT: Technical Barriers to Trade - affirmation of WTO rights and obligations, harmonization
- STE: State Trading Enterprises - competition authority requirements, nondiscrimination provisions
- AD: Antidumping provisions - retention of WTO rights
- CVM: Countervailing Measures - retention of WTO rights
- State Aid: Anti-competitive behavior assessment, annual reporting requirements
- Public Procurement: Progressive liberalization, national treatment, publication requirements
- TRIMs: Trade-Related Investment Measures - local content and export performance requirements
- GATS: Services liberalization provisions
- TRIPs: Intellectual property standards harmonization and enforcement

**Coding:** 
- 0 = Provision not mentioned or too generally mentioned
- 1 = Provision is mentioned in the agreement

### Sheet 2: WTO LE (WTO Covered Areas - Legal Enforceability)
Contains information on provisions that include legal enforcement mechanisms.

**Coding:**
- 0 = Provision not mentioned or not legally enforceable
- 1 = Provision mentioned and legally enforceable but explicitly excluded from dispute settlement
- 2 = Provision mentioned and legally enforceable with dispute settlement provisions

### Sheet 3: WTO-X AC (WTO-X Areas - Agreements Commitment)
Contains provisions falling outside current WTO mandate.

**Key WTO-X Provision Areas:**
- Anti-Corruption: Regulations on criminal offenses affecting trade
- Competition Policy: Anti-competitive conduct measures, harmonization of competition laws
- Environmental Laws: Environmental standards, enforcement, sanctions
- IPR: Accession to international intellectual property treaties
- Investment: Legal frameworks, dispute settlement mechanisms, national treatment
- Labour Market Regulation: National labor market regulations, ILO commitments
- Movement of Capital: Capital liberalization, restrictions on new limitations
- Consumer Protection: Harmonization of consumer protection laws
- Data Protection: Information exchange, joint projects
- Agriculture: Technical assistance, modernization projects
- Approximation of Legislation: Application of EC/EU legislation
- Audio Visual: Industry promotion, co-production encouragement
- Civil Protection: Harmonized rules implementation
- Innovation Policies: Framework participation, technology transfers
- Cultural Cooperation: Joint initiatives, local culture promotion
- Economic Policy Dialogue: Exchange of ideas, joint studies
- Education and Training: Education level improvement measures
- Energy: Information exchange, technology transfer
- Financial Assistance: Granting and administration rules
- Health: Disease monitoring, health information systems
- Human Rights: Human rights respect provisions
- Illegal Immigration: Re-admission agreements, prevention measures
- Illicit Drugs: Addiction treatment, drug supply reduction
- Industrial Cooperation: Modernization assistance, credit access
- Information Society: Technology dissemination, training
- Mining: Information exchange, joint initiatives
- Money Laundering: Standards harmonization, technical assistance
- Nuclear Safety: Laws and regulations, radioactive materials supervision
- Political Dialogue: Convergence of positions on international issues
- Public Administration: Technical assistance, training programs
- Regional Cooperation: Cooperation promotion, technical assistance
- Research and Technology: Joint research projects, researcher exchange
- SME: Technical assistance, finance access facilitation
- Social Matters: Social security coordination, working conditions
- Statistics: Statistical methods harmonization
- Taxation: Fiscal system reform assistance
- Terrorism: Information exchange, research collaboration
- Visa and Asylum: Information exchange, legislation drafting

**Coding:**
- 0 = Provision not mentioned or too generally mentioned
- 1 = Provision mentioned in the agreement

### Sheet 4: WTO-X LE (WTO-X Areas - Legal Enforceability)
Contains legal enforceability information for WTO-X provisions.

**Coding (same as WTO LE):**
- 0 = Provision not mentioned or not legally enforceable
- 1 = Provision mentioned but legally enforceable exclusion from dispute settlement
- 2 = Provision mentioned and legally enforceable

### Sheet 5: Agreements
Contains metadata about each trade agreement.

**Variables:**
- Agreement: Official name of the agreement
- RTAID: WTO Regional Trade Agreements Database identifier
- WBID: World Bank DTA Database identifier
- Goods/Services: Coverage scope (Goods, Services, or both)
- Agreement Type: FTA (Free Trade Agreement), CU (Customs Union), EIA (Economic Integration Agreement), PSA (Partial Scope Agreement)
- Status: In Force, Inactive, or other status
- Signature Date: Date agreement was signed
- Ratification Date: Date agreement was ratified
- Entry Into Force: Date agreement became effective
- Agreements Modified: Whether the agreement has been modified
- Bilateral: Whether agreement is bilateral or plurilateral
- Region: Geographic region(s) covered
- Accession: Whether agreement involves accession of new members
- Parties to Agreement: Countries/entities party to the agreement
- Current Parties: Current members after accessions
- Accession Information: Details on when accessions occurred
- Entry Year: Year agreement entered into force

### Sheet 6: Bilateral Information
Contains yearly observations at the bilateral country pair level.

**Variables:**
- iso1: ISO 3-letter code for first country
- iso2: ISO 3-letter code for second country
- rtaname: Name of the trade agreement
- WBID: World Bank identifier
- year: Observation year (1970-2023)
- Economy1: First country name
- Economy2: Second country name
- type: Agreement type classification
- entry: Year agreement entered into force for this pair

---

## Variable Coding and Interpretation

### Agreement Coverage Classification

**WTO Covered Areas (WTO AC/WTO LE):**
Provisions falling under the current mandate of the WTO and already subject to some form of commitment in existing WTO agreements.

**WTO-X Areas (WTO-X AC/WTO-X LE):**
Provisions and obligations that extend beyond the current WTO mandate, representing deeper trade integration.

### Enforceability Categories

The distinction between AC and LE sheets reflects important differences in implementation:

- **AC (Agreement Commitment) = 1:** Provision is present but may not be legally binding or enforceable
- **LE (Legal Enforceability) = 1:** Provision is legally binding but may be explicitly excluded from dispute settlement
- **LE = 2:** Provision is both legally binding and enforceable through dispute settlement mechanisms

---

## Key Provision Areas for Environmental Research

The DTA database is particularly valuable for researchers studying environmental provisions in trade agreements:

### Environmental Laws Variable (WTO-X LE)
Includes development of environmental standards, enforcement of national environmental laws, and establishment of sanctions for violations. Coded 0/1/2 for enforceability levels.

### Related Provisions

Environmental scholars should also consider:
- **Labour Market Regulation:** Often paired with environmental commitments in deep agreements
- **Competition Policy:** May affect environmental enforcement through market mechanisms
- **Investment Provisions:** Important for investor-state protections in environmental disputes
- **Agricultural Provisions:** Connected to environmental standards in trade

---

## Data Quality Notes

1. **Coverage:** The database covers agreements notified to the WTO plus some regional agreements
2. **Temporal Scope:** Bilateral observations available from 1970-2023
3. **Classification:** Provisions are coded based on agreement text analysis
4. **Enforceability:** Legal enforceability assessment is based on dispute settlement provisions

---

## Additional Resources

- WTO Regional Trade Agreements Database: https://rtais.wto.org/
- World Bank Trade Database: https://www.worldbank.org/en/topic/trade
---

## Notes for Researchers

### For China Trade Research
The database includes all major Chinese trade agreements (bilateral and regional) with detailed content provisions. The Environmental Laws variable is particularly useful for analyzing environmental commitments in China's trade agreements.

### For Econometric Analysis
The bilateral-year structure allows for:
- Gravity model specifications with fixed effects
- Difference-in-differences analysis of agreement impacts
- Interaction term analysis for specific provisions
- Time-variant treatment effects estimation
