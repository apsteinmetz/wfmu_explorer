---
title: "WFMU Playlist Explorer"
version: 0.40
date: "November 4, 2017"
output: html_document
---
![](https://wfmu.org/wp-content/uploads/2016/03/BadgeCourage.png) 

## About WFMU Playlist Explorer
WFMU.ORG, a free-form, listener-supported radio station, maintains a huge trove of past playlists from many DJ's radio shows.  More recently, web-only programming has been added to this.  This dataset offers lots of opportunities for analysis.  I scraped all the playlists I could from the web site and started asking questions.  The data set is here for your own explorations. It represents *over a million plays spanning decades!*

The web scraping and data-cleaning process was the most time consuming part of the exercise. Playlist tables are not in a consistent format and my HTML skills are rudimentary.  Some DJs are ommitted entirely because their playlist formats are too weird (working on it, maybe). I cleaned up the raw data to reduce errors and create consistency.  DJ's are inconsistent in how they enter artist names.  There are 12 different ways Andy Breckman can misspell "Bruce Springsteen"  I take a stab at fixing some of the glaring errors. I'm sure I missed many. Additionally, many artist names are variant due to collaborators with "featuring," "and the," "with," etc. in the name.  I condense the first two words of every artist name into an *ArtistToken* and drop the rest. In a very few cases the air date is clearly wrong. I strip those shows out.

There is a final step which is really a judgement call.  Many DJs have signature songs to open and/or close their shows.  Including these skews the play count for songs.  I have chosen to strip those out, or try to.  Songs where one DJ accounts for just about all the plays are stripped out as well.  This is the ultimate data set I use here.

The end result is an reasonably accurate but incomplete record of all the playlists available at WFMU.ORG as of September 2017.  

The code used for scraping,cleaning and analyzing is available at https://github.com/apsteinmetz/wfmu.

The code for this web app is at https://github.com/apsteinmetz/wfmu_explorer.git.

Thanks to station manager, Ken Freedman, for giving me permission to scrape the site.

wfmu.org is powered by KenzoDB ( http://kenzodb.com ) from Ken Garson Systems. Nice!

This site is powered by Shiny by RStudio (https://www.rstudio.com/) and written in the R data science lanugage.

**WFMU is listener supported!  [PLEDGE HERE!](https://pledge.wfmu.org/donate?step=landing)**

-- Art Steinmetz (apsteinmetz@yahoo.com)

## Change log

*Changes in 0.40: Added song tab to analyze artists and DJs who played a particular song title.

*Changes in 0.38:Added display of all variants of artist name included in artist token.

*Changes in 0.37:Redid single artist tab.  Artist selection much streamlined.

*Changes in 0.36: Multi-select on artist tab.  Many artist entries are misspelled.  Ideally, you'll combine variants.  You can now select, say, "DustySpringfield" and "DusySprinfield" if you start your search with "du."  Searching for "el" will let you select "ELO and "ElectricLight" (as in Orchestra).

Added a multi-artist tab.  Here you can compare more than one artist's plays over time.  At Ken's request the sizing of the plot is 710x355.  You can right-click "save as" in your browser to pull the image for further use.

*Changes in 0.35: Bug fixes. On/Off sched filtering fixed.  Data Set now includes many more shows where playlist links extended in to past years.  Known missing shows are Doug Shulkind and Greasy Kid Stuff.

*Changes in 0.34: Bug fixes. Date sliders were losing January!  

*Changes in 0.33: Clue portion of artist name can be any word in the name, not just the first.

*Changes in 0.32: UI more responsive.

*Changes in 0.31: create two-step process for selecting artists on artists tab.  The number of unique artists is more then 150 thousand (alas, many are misspellings).  That is too many for a drop-down list so now there is a two-step process to narrow down the list.  It's a bit clunky, I know.

*Changes in 0.30: includes artist play count history by DJ.

*Changes in 0.22: includes 2-DJ comparison.

*Changes in 0.21: includes DJ similarity.

*Changes in 0.20: includes DJ top plays.

