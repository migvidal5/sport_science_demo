## 🏃‍♂️ Sport Science Demo – Shiny App

This repository showcases a basic Shiny application built with R, aimed at demonstrating how to structure and develop interactive web applications for sport science data. It serves as a learning resource for those interested in building Shiny apps from scratch.

Please note that **sport_science_demo_folder** is in a standard directory structure expected by Shiny. Renaming or moving the files in here will result in issues deploying the app.


## 📁 Repository Structure
#### **create_dummy_data.R**
1.  A script designed to generate synthetic sport science data. This is useful for testing and demonstrating the app without relying on real datasets. Included is a formula to calculate EWMA ACWR.
#### sport_science_demo folder
1. **ui.R**
   - Defines the user interface of the app — including layout, inputs, and outputs that the user interacts with.
2. **server.R**
   - Contains the server-side logic — i.e., how data is processed and how outputs react to inputs.
3. **global.r**
   - File to load libraries, data, and theme for app to reference. 	
3. **data/**
   - folder with data from create_dummy_data.R in a .rda file.
4. **www/**
   - folder with stored images for app to reference

