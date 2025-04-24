🏃‍♂️ Sport Science Demo – Shiny App

This repository showcases a basic Shiny application built with R, aimed at demonstrating how to structure and develop interactive web applications for sport science data. It serves as a learning resource for those interested in building Shiny apps from scratch.

⸻

📁 Repository Structure

create_dummy_data.R
	•	Generates synthetic sport science data for testing and demonstration purposes.
	•	Includes a formula to calculate EWMA ACWR (Exponentially Weighted Moving Average Acute:Chronic Workload Ratio).

sport_science_demo/
	•	ui.R
Defines the user interface of the app — including layout, inputs, and outputs that users interact with.
	•	server.R
Contains the server-side logic — how data is processed and how outputs respond to user input.
	•	data/
Stores generated .rda data files from create_dummy_data.R.
	•	www/
Contains image assets used within the Shiny app (e.g., logos or background graphics).
