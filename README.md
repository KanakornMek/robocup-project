# RoboCup Simulation (Symbolic AI)

A rule-based soccer simulation implemented using **Symbolic AI** and **Logic Programming with Prolog**, featuring a Python `tkinter` GUI for visualization.

This project was developed as part of a Symbolic AI course. It demonstrates key concepts such as knowledge representation, logical reasoning, rule-based decision-making, search planning, and constraint satisfaction within a dynamic environment.

## 📋 Table of Contents

* [Project Overview](#-project-overview)
* [Features](#-features)
* [Prerequisites](#-prerequisites)
* [Installation](#-installation)
* [Usage](#-usage)
* [AI Implementation Details](#-ai-implementation-details)
* [File Structure](#-file-structure)

## ⚽ Project Overview

The simulation models a soccer match between two teams (Team 1 and Team 2) on a  field. The "brain" of the simulation is written entirely in **SWI-Prolog**, which handles the state of the world, player decision-making, and game physics. **Python** is used to bridge these logical queries to a graphical user interface.

The system represents the soccer field, players, ball, goals, and roles symbolically using Prolog facts and relations.

## ✨ Features

* **Symbolic Knowledge Representation:** Players, ball positions, and field dimensions are stored as dynamic Prolog facts.
* **Intelligent Agent Behaviors:**
* **Goalkeepers:** Protect the goal and catch loose balls within range.
* **Forwards:** Chase the ball, position themselves offensively, and shoot when in range.
* **Defenders:** Intercept opponents and cover defensive lines.
* **Advanced Game Mechanics:**
* **Stamina System:** Players consume energy when moving and regenerate it when idle. Speed is throttled by current stamina.
* **Smart Passing:** Agents calculate the "best" teammate to pass to based on distance to goal, openness (distance from opponents), and teammate stamina.
* **Spatial Reasoning:** Agents use `find_open_space` algorithms to avoid opponents while moving towards objectives.
* **Tackling:** A probability-based system allows defenders to steal the ball from an opponent.
* **Visualization:** A Python `tkinter` interface that updates in real-time based on Prolog simulation rounds.

## 🛠 Prerequisites

To run this project, you need the following installed:

1. **SWI-Prolog:** Ensure the binary is in your system PATH.
* *Download:* [SWI-Prolog.org](https://www.swi-prolog.org/Download.html)
2. **Python 3.x**
3. **PySWIP:** A Python bridge for SWI-Prolog.

## 📦 Installation

1. **Clone the repository:**
```bash
git clone https://github.com/your-username/robocup-symbolic-ai.git
cd robocup-symbolic-ai
```


2. **Install Python dependencies:**
```bash
pip install pyswip tk
```

## 🚀 Usage

There are two ways to run the simulation:

### Method 1: Python GUI

This uses the Python script to visualize the Prolog logic.

```bash
python robocup.py

```

1. Enter the number of rounds to simulate (e.g., `500`).
2. Click **Run Simulation**.
3. Use the **Pause/Resume** button to control the flow.

### Method 2: Prolog Standalone

If you wish to test the logic directly in the Prolog console or use the legacy XPCE interface included in `robocup.pl`:

```prolog
swipl robocup.pl
?- run_simulation(10).

```

## 🧠 AI Implementation Details

The core logic is contained in `robocup-py.pl`.

### 1. Decision Making

The logic separates agents into two states:

* **With Ball (`decide_action_with_ball/1`):** The agent evaluates if they should **Shoot** (if close to goal and line-of-sight is clear), **Pass** (if a teammate is in a better position), or **Dribble** (move towards goal).
* **Without Ball (`decide_action_without_ball/1`):**
* *Offensive Move:* If a teammate has the ball, find open space closer to the opponent's goal.
* *Defensive Move:* If an opponent has the ball, position between the ball carrier and the goal.



### 2. Search & Planning

The system uses `find_open_space/6` to calculate valid coordinates. It iterates through radii around a point to find a location that satisfies:

1. Is within field bounds.
2. `\+ is_near_opponent`: No opponents are within a specific proximity threshold.
3. `\+ is_near_teammate`: Prevents bunching up.

### 3. Physics & Rules

* **Movement:** Uses vector normalization to calculate movement steps. Logic includes `clamp/4` to ensure players stay within field boundaries.
* **Scoring:** Checks if the ball coordinates cross the Y-plane of the goal lines.

## 📂 File Structure

* `robocup.py`: The main Python entry point. Initializes the GUI and bridges to Prolog.
* `robocup-py.pl`: The "backend" Prolog logic optimized for Python integration (removes internal XPCE GUI calls).
* `robocup.pl`: The original standalone Prolog file containing the logic and an internal XPCE GUI implementation.
