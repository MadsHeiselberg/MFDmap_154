🗺️ Microflora Danica Map — Interactive Geographic Visualization

Live map: https://madsheiselberg.github.io/MFDmap_154/

An interactive map visualizing the geographic distribution of Microflora Danica sampling locations.
This repository contains the code and data processing pipeline that transforms the raw supplementary data from the manuscript into a clean dataset and a rendered HTML map.

✨ Features

🌍 Interactive map built with R Markdown

🧹 Data cleaning pipeline for manually curated location naming

📍 Accurate GPS visualization of sample origins

📦 Automatically deployed via GitHub Pages

📁 Repository Structure
├── index.html                # Main HTML webpage containing the interactive map
│
├── Rmarkdown/                # RMarkdown-based map generation
│   ├── MicrofloraDanica.Rmd  # Primary RMarkdown file generating the map
│   ├── process_data.R        # Filters + cleans raw data into final map-ready dataset
│   │
│   └── Data/
│       ├── 41564_2025_2062_MOESM3_ESM.xls     # Raw data from publication (locations + metadata)
│       ├── data_clean_bynavnetjek.csv         # Manually verified Danish city name translations
│       └── shinyapp_data_done.csv             # Final cleaned dataset used in map rendering
│
└── README.md

📄 Licens og ophavsret

Dette projekt anvender Leaflet, et open source JavaScript-bibliotek and leaflet R package from the  til interaktive kort.

Leaflet er udviklet af Volodymyr Agafonkin og CloudMade, og distribueres under BSD 2-Clause License:

Copyright (c) 2010–2025, Volodymyr Agafonkin
Copyright (c) 2010–2011, CloudMade
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE.
