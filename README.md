<!DOCTYPE html>
<html>
<head>
  <title>README</title>
</head>
<body>

<h1>EARTH ASTEROID IMPACT PROBABILITY</h1>
<h2>FINAL PROJECT</h2>
<h3>ACM40960 - Projects in Maths Modelling</h3>
<p><strong>Author:</strong> Abhimanyu Premachandran Nair</p>

<hr>

<h2>Project Description</h2>
<p>This project involves the mathematical modeling of the trajectory of an asteroid in the solar system, considering the gravitational interactions between the Sun, Earth, and Jupiter. The main objective is to determine whether the asteroid impacts the Earth during its journey.</p>

<h2>Code Overview</h2>
<p>The provided code performs the following steps:</p>
<ol>
  <li>Loads necessary R packages for data visualization and analysis.</li>
  <li>Defines constants and initial conditions for the simulation.</li>
  <li>Performs backward integration to trace the trajectory of the asteroid from a pre-chosen radius (ra) towards an earlier time.</li>
  <li>Performs forward integration to trace the trajectory of the asteroid from ra towards a later time.</li>
  <li>Checks for impact between the asteroid and Earth during forward integration.</li>
  <li>Plots the trajectories of Earth and the asteroid.</li>
  <li>Runs simulations for different initial conditions and delta values.</li>
  <li>Introduces Jupiter to the system and repeats the integration and impact analysis.</li>
</ol>

<h2>Usage Instructions</h2>
<p>To run the code:</p>
<ol>
  <li>Ensure you have R and the necessary packages (ggplot2, gganimate) installed.</li>
  <li>Copy and paste the code into an R script or environment.</li>
  <li>Execute the code to visualize the trajectory and analyze impact scenarios.</li>
</ol>

<h2>Results</h2>
<p>The code generates plots and outputs indicating whether the asteroid impacts the Earth under different conditions and parameters.</p>

<h2>Disclaimer</h2>
<p>This code is for educational purposes and simulation only. Results may vary depending on the model assumptions and parameters used.</p>

<hr>

</body>
</html>