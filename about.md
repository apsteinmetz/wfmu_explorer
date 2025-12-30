---
title: "WFMU Playlist Explorer"
version: 1.1
date: "January 2026"
output: html_document
---

![](badge.png)

<!-- Global Site Tag (gtag.js) - Google Analytics -->

```{=html}
<script async src="https://www.googletagmanager.com/gtag/js?id=UA-107406537-1"></script>
```
```{=html}
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments)};
  gtag('js', new Date());

  gtag('config', 'UA-107406537-1');
</script>
```
## About WFMU Playlist Explorer

[WFMU.ORG](http:www.wfmu.org), a free-form, listener-supported radio station, maintains a huge trove of past playlists from many DJ's radio shows. More recently, web-only programming has been added to this. This dataset offers lots of opportunities for analysis. I scraped all the playlists I could from the web site and started asking questions. The data set is here for your own explorations. It represents *over a three million plays spanning decades!*

The web scraping and data-cleaning process was the most time consuming part of the exercise. Playlist tables are not in a consistent format and my HTML skills are rudimentary. Some DJs are ommitted entirely because their playlist formats are too weird (working on it, maybe). I cleaned up the raw data to reduce errors and create consistency. DJ's are inconsistent in how they enter artist names. There are 12 different ways Andy Breckman can misspell "Bruce Springsteen" I take a stab at fixing some of the glaring errors. I'm sure I missed many. Additionally, many artist names are variant due to collaborators with "featuring," "and the," "with," etc. in the name. I condense the first two words of every artist name into an *ArtistToken* and drop the rest. In a very few cases the air date is clearly wrong. I strip those shows out.

There is a final step which is really a judgement call. Many DJs have signature songs to open and/or close their shows. Including these skews the play count for songs. I have chosen to strip those out, or try to. Songs where one DJ accounts for just about all the plays are stripped out as well. This is the ultimate data set I use here.

The end result is an reasonably accurate but incomplete record of all the playlists available at WFMU.ORG as of the last web scraping.

The code used for scraping,cleaning and analyzing is available at <https://github.com/apsteinmetz/wfmu>.

The code for this web app is at <https://github.com/apsteinmetz/wfmu_explorer.git>.

Thanks to station manager, Ken Freedman, for giving me permission to scrape the site. This is a personal project, not offically WFMU-sponsored or approved.

wfmu.org is powered by KenzoDB ( <http://kenzodb.com> ) from Ken Garson Systems.

This site is powered by Shiny by Post (<https://www.posit.co/>) and written in the R data science lanugage.

**WFMU is listener supported!**
<style>
.iframe-container {
  position: relative;
  width: 100%;
  padding-bottom: 30%; /* aspect ratio of image */
  height: 0;
  overflow: hidden;
}

.iframe-container iframe {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  border: 0;
}
</style>

<div class="iframe-container">
  <iframe src="https://pledge.wfmu.org/pledge-widget"></iframe>
</div>

#### -- Art Steinmetz 
#### Contact: [apsteinmetz\@yahoo.com](mailto:apsteinmetz@yahoo.com))
#### More about me: [artsteinmetz.com](https://artsteinmetz.com)

## Change log
\* Changes in 1.1 Added option to exclude robot DJs and select by stream channel.  More playlist formats added to data set.  "Let's Encrypt" certificate added to ensure secure https connection.  Hopefully this will prevent browsers from blocking the app as "not secure."  Playlists file is getting pretty big - swtiched to Github LFS to handle it.

\*Out of Beta! Changes in 1.0 Links back to WFMU for station, DJ profile page, and DJ archived shows.  Added option to exclude "Wake 'n' Bake" since daily frequency of show distorts picture.

\*Changes in 0.91 Optimized all functions to perform best with duckdb.  Much faster, less crashy.

\*Changes in 0.9 Switched from arrow do duckdb database engine for speed. Added a new panel to show DJ's most distinctive artists.

\*Changes in 0.8 Switched from dtplyr to apache arrow for handling tables for speed. Updated to new versions of Ubuntu, R. Started using `renv` package to isolate app from package updates that might break it.

\*Changes in 0.7 Switched from dplyr to dtplyr for handling tables. dtplyr uses the data.table package for much faster speed. Updated to new versions of Ubuntu, R and Shiny,

Load playlist files from github when the app is started so that data updates will happen automatically whenever I rescrape the station's playlist files. Previously, I had to manually log into the server to update the files.

\*Changes in 0.6 Corona virus update. Rewrote almost entire app. It broke after a number of R package updates. I thought I was just updating the playlist data but tragedy ensued. Lesson: sandbox even trival updates. Switched to fancier worldcloud package. Included a couple more DJs who have irregular playlist formats on the web site. Many still missing.

\*Changes in 0.52: New Goth theme!

\*Changes in 0.51: Streamlined multi-artist selection tab.

\*Changes in 0.50: Moving from alpha to beta! Added playlists tab to display raw one or more raw playlists of a single DJ. Searchable and sortable! Signature songs are stripped out of the data set so as not to skew popularity measures elsehwere in the app. Ideally you should be able to toggle signature songs on and off. I'll think about this.

\*Changes in 0.40: Added song tab to analyze artists and DJs who played a particular song title.

\*Changes in 0.38:Added display of all variants of artist name included in artist token.

\*Changes in 0.37:Redid single artist tab. Artist selection much streamlined.

\*Changes in 0.36: Multi-select on artist tab. Many artist entries are misspelled. Ideally, you'll combine variants. You can now select, say, "Dusty Springfield" and "Dusy Sprinfield" if you start your search with "du." Searching for "el" will let you select "ELO and"Electric Light" (as in Orchestra).

Added a multi-artist tab. Here you can compare more than one artist's plays over time. At Ken's request the sizing of the plot is 710x355. You can right-click "save as" in your browser to pull the image for further use.

\*Changes in 0.35: Bug fixes. On/Off sched filtering fixed. Data Set now includes many more shows where playlist links extended in to past years. Known missing shows are Doug Shulkind and Greasy Kid Stuff.

\*Changes in 0.34: Bug fixes. Date sliders were losing January!

\*Changes in 0.33: Clue portion of artist name can be any word in the name, not just the first.

\*Changes in 0.32: UI more responsive.

\*Changes in 0.31: create two-step process for selecting artists on artists tab. The number of unique artists is more then 150 thousand (alas, many are misspellings). That is too many for a drop-down list so now there is a two-step process to narrow down the list. It's a bit clunky, I know.

\*Changes in 0.30: includes artist play count history by DJ.

\*Changes in 0.22: includes 2-DJ comparison.

\*Changes in 0.21: includes DJ similarity.

\*Changes in 0.20: includes DJ top plays.
