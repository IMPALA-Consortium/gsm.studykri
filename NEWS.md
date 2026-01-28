# gsm.studykri 0.2.1

- Added `vDbIntRandomRange` parameter to bootstrap functions (`Analyze_StudyKRI_PredictBounds()`, `Analyze_StudyKRI_PredictBoundsRef()`, `Analyze_StudyKRI_PredictBoundsRefSet()`) to support database backends that use large integer values for random number generation (e.g., Snowflake) (#7)


# gsm.studykri 0.2.0

- Added 10 additional KRI workflows (kri0005-kri0014) covering lab abnormalities, discontinuations, queries, data quality, screening failures, and I/E violations
- More carefull month to denominator matching for KRI with same denominator
- better implementation fo vMinDenominator to define minimal Denominator count to define start of study timeline

# gsm.studykri 0.1.0

- Initial release
- Bootstrap-based study-level KRI confidence intervals
- 4 base KRI workflows (AE, SAE, PD, IPD)
- Portfolio simulation capabilities
