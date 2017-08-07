# esaj
🇧🇷

## Overview

The `esaj` R package in a simple interface that allows you to download first
and second degree lawsuits from Brazil's multiple e-SAJ (Electronic Justice
Automation System) portals. Before `esaj` if you wanted to gather information
about lawsuits being processed by Brazil's state-level Judiciary, you would
have to go to each state's e-SAJ portal, manually input each lawsuit's ID,
and only then download the PDF with the information you wanted; now you can
simply run `download_esaj()`, and spend your valuable time analysing the data.

## Installation

To install `esaj`, run the code below:

```r
# install.packages("devtools")
devtools::install_github("courtsbr/esaj")
```

## Usage

`esaj` has only one exported function: `download_esaj()`. Its arguments are a
lawsuit ID and the path to the directory where the lawsuit should be downloaded.

```r
library(esaj)

# Download a lawsuit from Amazonas
download_lawsuit("02575182220138040001")
#> [1] "./02575182220138040001.html"

# Download a lawsuit from Santa Catarina
download_lawsuit("0303349-44.2014.8.24.0020", "./test")
#> [1] "./test/03033494420148240020.html"
```

Note that `download_esaj()` figures out the state where the lawsuit was filed
from the ID alone, so you don't even have to worry about finding out where
the lawsuit is from. Another important point is that the donwloaded files
have a set name that you cannot change; you can only alter the directory
where they will be saved (the default is the current working directory).

## Implemented TJs

Unfortunatelly `download_lawsuit()` doesn't yet work with all 27 TJs in
Brazil. Here is a list of the ones implemented:
- [ ] Acre (AC)
- [ ] Alagoas (AL)
- [ ] Amapá (AP)
- [X] Amazonas (AM)
- [X] Bahia (BA)
- [ ] Ceará (CE)
- [ ] Distrito Federal (DF)
- [ ] Espírito Santo (ES)
- [ ] Goiás (GO)
- [ ] Maranhão (MA)
- [ ] Mato Grosso (MT)
- [ ] Mato Grosso do Sul (MS)
- [ ] Minas Gerais (MG)
- [ ] Pará (PA) 
- [ ] Paraíba (PB)
- [ ] Paraná (PR)
- [ ] Pernambuco (PE)
- [ ] Piauí (PI)
- [ ] Rio de Janeiro (RJ)
- [ ] Rio Grande do Norte (RN)
- [ ] Rio Grande do Sul (RS)
- [ ] Rondônia (RO)
- [ ] Roraima (RR)
- [X] Santa Catarina (SC)
- [ ] São Paulo (SP)
- [ ] Sergipe (SE)
- [ ] Tocantins (TO)
