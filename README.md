# [Does the Exchange Rate Respond to Monetary Policy in Mexico? Solving an Exchange Rate Puzzle in Emerging Markets](https://onlinelibrary.wiley.com/doi/full/10.1111/jmcb.13032)

by Pavel Solís (pavel.solis@gmail.com)


## System Features
The results in the paper were generated using the following:
- Operating systems: macOS 12.2.1, Windows 10
- Software: Stata 17
- Add-ons: scheme-modern, regsave, texsave, coefplot
- Expected running time: 5 min


## Contents of Folder
README.md (this file)
- Codes folder with the following subfolder:
	- Analysis: replication script
- Data folder with the following subfolder:
	- Analytic: analysis dataset
- Docs folder with the following subfolders:
	- Paper folder: files that make up the manuscript
	- Figures folder: files with the figures
	- References folder: .bib file with cited references


## Dataset
With the following exceptions, the variables in the dataset represent (percent) changes (\* is a wildcard):
- date\* contain dates
- idx\*, regular and usjobsday are binary variables
- levelwti reports end-of-day values

The following variables report intraday (percent) changes in 30-min windows:
- gmxn\*yr, mpsw28t, mpswc, trm\*, usdmxn

Some variables use the following suffixes:
- _ttwd for intraday (percent) changes in 50-min windows
- _ttdm for daily (percent) changes


## Instructions for Replication
Execute the codes.do file to replicate the results in the paper
- The codes store the tables and figures in the respective folder

Execute the paper.tex file to generate the PDF version of the manuscript
