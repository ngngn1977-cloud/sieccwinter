SIECC WINTER PROJECT FOLDER - READ ME FIRST

What to copy into this folder
  data\         the study data CSV (the canonical CSV made by prepare_private_csv.ps1).
  references\   optional: the reference bundle folders and the historical run folders
                (the failed ANOM run, the accepted paper run, the accepted Batch B run),
                the post hoc reference files (V1.4 and archive-engine files listed in
                config/posthoc_sensitivity_protocol_v2.json; including the eleven GIS files of the
                analysed IMS run, participant- and station-level, local only) and, for the nearest-station
                check, the nearest-station export CSV(s) as exported (participant-level,
                local only; every declared export variant found is run separately).
  outputs\      leave empty: every run writes a new folder here (run_<date>_<time>_<id>).

The subfolders are only a convenience. The program recognises every file by its
content (a SHA-256 fingerprint compared with the declared values), not by its name
or by the folder it sits in. You may rename files or put them in other subfolders.
Two copies of the same file make the choice ambiguous: the program then stops and
lists both. Keep one copy. Do not put the data CSV directly in this top folder;
put it in a subfolder such as data\.

How to run
  1. Double-click RUN_SIECC.cmd in the package folder.
  2. Choose this project folder in the window that opens.
  3. Read the plan (files found, steps that will run or will be NOT_TESTED, free
     disk space, estimated time) and type Y to start.
  At the end the run folder opens. Open RESULTS_SUMMARY.html first.

What happens with only the data CSV
  The paper analysis runs (mode ANALYSIS). Every validation step is NOT_TESTED,
  which never counts as passed.

Privacy
  Everything is written inside this project folder. The outputs contain aggregate
  results only. Never send the data CSV to anyone; send only the files the guides
  name as aggregate.
