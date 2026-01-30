# gsm.studykri 0.2.1

- Established compatibility with snowflake backend by:
- Added `vDbIntRandomRange` parameter to bootstrap functions (`Analyze_StudyKRI_PredictBounds()`, `Analyze_StudyKRI_PredictBoundsRef()`, `Analyze_StudyKRI_PredictBoundsRefSet()`) to support database backends that use large integer values for random number generation (e.g., Snowflake) (#7)
- Added `nMinGroups` parameter to `Analyze_StudyKRI_PredictBoundsRefSet()` to allow passing pre-calculated minimum group counts, avoiding expensive `collect()` operations on database backends
- Added `strMinGroupsCol` parameter to `Analyze_StudyKRI_PredictBoundsRef()` to specify custom column name for minimum group counts in StudyRef tables (default: "MinGroups")
- Added `bSkipValidation` parameter to `JoinKRIByDenominator()` to disable validation checks that require `collect()` operations for improved performance with database backends


# gsm.studykri 0.2.0

- Added 10 additional KRI workflows (kri0005-kri0014) covering lab abnormalities, discontinuations, queries, data quality, screening failures, and I/E violations
- More carefull month to denominator matching for KRI with same denominator
- better implementation fo vMinDenominator to define minimal Denominator count to define start of study timeline

# gsm.studykri 0.1.0

- Initial release
- Bootstrap-based study-level KRI confidence intervals
- 4 base KRI workflows (AE, SAE, PD, IPD)
- Portfolio simulation capabilities
