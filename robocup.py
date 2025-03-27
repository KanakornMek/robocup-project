import tkinter as tk
from pyswip import Prolog

# Initialize the Prolog interpreter and consult the Prolog file (ensure the file is correct).
prolog = Prolog()
prolog.consult('robocup-py.pl')  # Path to your Prolog file

# Tkinter GUI Setup
class SoccerSimulationApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Prolog Soccer Simulation")

        # Canvas to draw the field and players
        self.canvas = tk.Canvas(root, width=500, height=250, bg="white")
        self.canvas.pack()

        # Simulation controls
        self.rounds_label = tk.Label(root, text="Rounds:")
        self.rounds_label.pack()
        self.rounds_entry = tk.Entry(root)
        self.rounds_entry.pack()
        self.run_button = tk.Button(root, text="Run Simulation", command=self.run_simulation)
        self.run_button.pack()

        self.rounds = 0
        self.current_round = 0

    def run_simulation(self):
        try:
            self.rounds = int(self.rounds_entry.get())
            if self.rounds <= 0:
                return
        except ValueError:
            return

        self.current_round = 0
        self.simulate_round()  # Start the first round

    def simulate_round(self):
        # Run the Prolog simulation for one round
        print("Simulating round...", self.current_round + 1)
        for solution in prolog.query("simulate_round"):
            pass  # Execute the Prolog query to simulate a round

        self.update_gui()  # Update the GUI after this round

        self.current_round += 1
        if self.current_round < self.rounds:
            # Schedule the next round after a small delay
            self.root.after(50, self.simulate_round)  # 50ms delay
        else:
            print("Simulation finished.")

    def update_gui(self):
        self.canvas.delete("all")  # Clear the canvas
        print("Updating GUI...")

        # Draw the field
        self.draw_field()

        # Draw the players
        self.draw_players()

        # Draw the ball
        self.draw_ball()

    def draw_field(self):
        # Draw the field (green rectangle)
        self.canvas.create_rectangle(50, 25, 450, 225, fill="green", outline="black")

    def draw_players(self):
        # Draw players (red for team 1, blue for team 2)
        for solution in prolog.query("player(Team, Role, position(X, Y), _)"):
            team = solution["Team"]
            role = solution["Role"]
            x = solution["X"]
            y = solution["Y"]

            # Convert Prolog field coordinates to GUI coordinates
            cx, cy = self.convert_coords(x, y)

            color = "red" if team == "team1" else "blue"
            self.canvas.create_oval(cx - 10, cy - 10, cx + 10, cy + 10, fill=color)

            # Display the role text
            self.canvas.create_text(cx, cy - 15, text=role, fill="black")

    def draw_ball(self):
        # Get ball position and convert to GUI coordinates
        for solution in prolog.query("ball(position(X, Y))"):
            x = solution["X"]
            y = solution["Y"]

            cx, cy = self.convert_coords(x, y)
            self.canvas.create_oval(cx - 8, cy - 8, cx + 8, cy + 8, fill="black")

    def convert_coords(self, x, y):
        # Convert the field coordinates to GUI canvas coordinates
        cx = 50 + x * 4
        cy = 25 + y * 4
        return cx, cy

if __name__ == "__main__":
    # Set up the Tkinter window
    root = tk.Tk()
    app = SoccerSimulationApp(root)
    root.mainloop()
