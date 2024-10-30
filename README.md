── Preparing for deployment ────────────────────────────────────────────────────
✔ Re-deploying "sdaa" to "server: rsc.pfizer.com / username: chinns37"
ℹ Looking up application with id 10309...
✔ Found application <https://rsc.pfizer.com/content/e7c64d1f-5a53-4851-98ba-14867ccc5c93/>
ℹ Bundling 22 files: '.Rbuildignore', '.RData', 'Data/c1071003_pf-06863135_serum_mock_dft_v2 (1) (version 1).xlsb.csv', 'Data/c1071003_pf-06863135_serum_mock_dft_v2.xlsx', 'DESCRIPTION', 'global.R', 'LICENSE', 'Logo/pfizer.png', 'README.md', 'SDAA/app.R', 'server.R', 'ui.R', 'www/pdash.css', 'modules/01_PREQUISITES.R', 'modules/02_SDAA_DASHBOARD.R', 'modules/BckupLoad_Data_Tab.R', 'modules/Data_Insights2.R', 'modules/Data_Insights3.R', …, 'modules/Normal_Distribution.R', and 'modules/packages.R'
ℹ Capturing R dependencies with renv
✔ Found 115 dependencies
Error in `createAppManifest()`:
! All packages must be installed from a reproducible location.
✖ Can't re-install packages installed from source: bs4Dash.
ℹ See `rsconnect::appDependencies()` for more details.
Backtrace:
    ▆
 1. └─rsconnect::deployApp(...)
 2.   └─rsconnect:::bundleApp(...)
 3.     └─rsconnect:::createAppManifest(...)
 4.       └─rsconnect:::bundlePackages(...)
 5.         └─rsconnect:::checkBundlePackages(deps, call = error_call)
 6.           └─cli::cli_abort(...)
 7.             └─rlang::abort(...)
Execution halted
