# Parker's Stacked Bar Plot for Taxonomic Data Makerer V3.5

## Overview
Parker's Stacked Bar Plot for Taxonomic Data Makerer is a Shiny web application designed to help users create stacked bar plots of taxonomic data. The app takes an ASV abundance table as input and allows users to filter and visualize the data at various taxonomic levels.

## What's New in Version 3.5

### New Features
1. **Improved Navigation:**
   - Dropdown menus for `Select Filter Level` and `Select Plot Level` are now sorted alphabetically, making it easier to navigate through taxonomic levels.
   - `Select Filter Name` dropdown now includes both the selected taxonomic level and the level above it for better context (e.g., `p__Proteobacteria c__Alphaproteobacteria`).

2. **Enhanced Plot Readability:**
   - The plot still shows relative abundances as percentages on the y-axis.
   - The numbers displayed within the bars now represent the absolute number of reads, providing a clearer understanding of the data.

3. **Taxonomic Data Handling:**
   - Taxa labeled as `x__NA` are removed from the `Select Filter Name` dropdown but are still included in the plotting and analysis under the category `Other`.

### Bug Fixes
- Fixed an issue where the previous selection in `Select Filter Name` persisted when changing the `Select Filter Level`, causing confusion.

## Instructions

1. **Upload your ASV Abundance Table:**
   - Click the "Browse" button and select your Excel file containing the ASV abundance data.

2. **Select Filter Level:**
   - Choose the taxonomic level you want to filter by (e.g., Domain, Phylum, Class).

3. **Select Filter Name:**
   - Select the specific name within the chosen filter level. The dropdown includes both the selected level and the level above it for better context.

4. **Select Plot Level:**
   - Choose the taxonomic level you want to plot (e.g., Phylum, Class).

5. **Create Plot:**
   - Click the "Create Plot" button to generate the stacked bar plot. The plot will display relative abundances with the absolute number of reads shown within the bars.

6. **Save Plot:**
   - Click the "Save Plot" button to save the generated plot as a PNG file.

## Notes
- The application uses relative abundances for the y-axis of the plot, but the numbers within the bars represent absolute reads.
- `x__NA` taxa are included in the analysis under the category `Other`.

## Dependencies
- `shiny`
- `readxl`
- `dplyr`
- `ggplot2`
- `tidyr`
- `shinythemes`
