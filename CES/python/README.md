Study of the effect of parenting on political attitudes, based on the 2010-2014 waves of the [Cooperative Election Study](https://cces.gov.harvard.edu/pages/welcome-cooperative-congressional-election-study) (CES).

* `run.py` Script to run analysis. Most output is printed to a set of text files in the `output` directory.
* `parents_politics_panel.py` Defines `ParentsPoliticsPanel` and related classes, to provide structure for analysis of attitudes within a panel survey.
* `ces.py` Implements `CESPanel`, a subclass of `ParentsPoliticsPanel` that uses CES data.
* `yougov.py` Skeletal beginning of `YouGovPanel`, a second implementation of `ParentsPoliticsPanel` using the Voter Study Group's [VOTER Survey](https://www.voterstudygroup.org/data/voter-survey).