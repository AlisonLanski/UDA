---
title: "Analyzing Text"
author: "Unstructured Data Analytics"
output:
  pdf_document:
  toc: yes
always_allow_html: true
html_document:
  toc: yes
toc_float: yes
theme: spacelab
highlight: tango
df_print: paged
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

```{r echo=TRUE, message=FALSE, warning=FALSE}
library(tidyverse)
library(stringr)
```