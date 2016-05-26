# `vgsample`: A representative sample of video games.

### What is `vgsample`?

`vgsample` is one half of the Digital Protagonist Research Toolkit (DPRT)
created by our team University of Washington iSchool students, the Datavores.
The DPRT is a collection of pair of data products designed to remove barriers
to video game research, especially research interested in examining trends in
video game protagonists.

One key barrier to examining video game protagonists - and video games more
broadly - is that there are a LOT of video games. If you want to explore
trends in games, there is a an overwhelming number to tackle.

When a population is too large or expensive to study in its entirety, the
standard strategy is to gather a random sample from that population and study
that instead. However, to sample from a population you need access to list
of population members, coupled with any data you need to filter or identify
subgroups in that population.

Unfortunately, this is incredibly difficult to do with video games due to the
siloed, non-standardized nature of video game libraries. There is no "official"
library of video games and no widely agreed upon format for data about video
games.

`vgsample` is the public packaging of our team's efforts to build a solution
to this problem.

### Okay, but what is `vgsample` technically?

For persons who have never heard of R and/or want nothing to do with such a
lovely/awesome programming language, `vgsample` is a GitHub repo that houses
the sample we created and documents describing how that sample came to be.

For friends of R, `vgsample` is an R package containing a dataframe with the
game sample coupled with supporting functions and vignettes.

In either case, `vgsample` is a representative sample of video games stratified
by first release year. This sample is coupled with the code used to produce the
sample along with descriptions of the data sources our team drew from, our
cleaning and sampling processes, and our rationale.

### How was the sample made? (The short version).

Our team aggregrated four major public libraries of game data (GiantBomb,
UVList, MobyGames, and the now-defunct AllGames) and extracted a random sample
of games from each year of game data based on each game's initial release year.

You can dig into the documents in the vignettes folder for details.

### Why was the sample made?

This sample is designed to assist all parties interested in studying trends in
video games over time by providing a clean, manageable collection of games to
study which can be used to make inferences about the larger game population.

### Where do I start?

If you don't want to mess around with R, you can simply download the video game
sample data and start reading the hosted PDFs.

If you're hungry for the R version:

```
devtools::install_github("")
