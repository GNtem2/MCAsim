# MCAsim

This is a description of works on computer model of the brain. It involves experimental simulation of anterior circulation stroke. The results can be found at https://gntem2.github.io/MCAsim/

The paper on this work has been published at https://www.frontiersin.org/articles/10.3389/fneur.2014.00176/full

A follow up paper involves simulaton of clot retrieval procedure is published at https://www.frontiersin.org/articles/10.3389/fneur.2020.00773/full. It uses data computer_long_format190118.Rda. A shiny app related to this publication is available at https://gntem3.shinyapps.io/ecrsim


## Git bash
Issues occurred because of existing folder

#### Initiate Git
git init 
git remote add origin https://github.com/GNtem2/MCAsim.git

### Updates were rejected because the tip of your current branch is behind
git branch -u origin/master master

### fatal: refusing to merge unrelated histories
git pull --allow-unrelated-histories

### add files to git
git add README.md 
git commit -m "update" 
git push -u origin master