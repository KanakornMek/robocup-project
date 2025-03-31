import tkinter as tk
from pyswip import Prolog

# Initialize the Prolog 
prolog = Prolog()
prolog.consult('robocup-py.pl') 

# Tkinter GUI Setup
class SoccerSimulationApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Prolog Soccer Simulation")

        # Canvas to draw the field and players
        
        field_size_query = list(prolog.query("field(size(W, H))"))
        self.field_width = field_size_query[0]["W"]
        self.field_height = field_size_query[0]["H"]
        self.offset_x = 10
        self.offset_y = 10
        self.canvas = tk.Canvas(root, width=(self.field_width + 2*self.offset_x), height=(self.field_height + 2*self.offset_y), bg="white")
        self.canvas.pack()

        # Simulation controls
        self.rounds_label = tk.Label(root, text="Rounds:")
        self.rounds_label.pack()
        self.rounds_entry = tk.Entry(root)
        self.rounds_entry.pack()
        self.run_button = tk.Button(root, text="Run Simulation", command=self.run_simulation)
        self.run_button.pack(pady=5)  

        self.paused = False
        self.pause_button = tk.Button(root, text="Pause", command=self.toggle_pause)
        self.pause_button.pack(pady=5) 

        self.rounds = 0
        self.current_round = 0
        self.update_gui()  # Initial GUI update

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
            if not self.paused:
                self.root.after(50, self.simulate_round)  # 50ms delay
        else:
            print("Simulation finished.")

    def toggle_pause(self):
        self.paused = not self.paused
        if self.paused:
            self.pause_button.config(text="Resume")
        else:
            self.pause_button.config(text="Pause")
            self.simulate_round()

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
        self.canvas.create_rectangle(self.offset_x, self.offset_y, self.field_width+self.offset_x, self.field_height+self.offset_y, fill="green", outline="black", width=2)

        goal_height = 100
        goal_width = 10
        goal_center_y = self.field_height / 2
        goal_top_y = goal_center_y - (goal_height / 2)

        # Team 1 goal (left side)
        self.canvas.create_rectangle(self.offset_x, self.offset_y + goal_top_y, self.offset_x + goal_width, goal_top_y + self.offset_y + goal_height, fill="white", outline="")

        # Team 2 goal (right side)
        self.canvas.create_rectangle(self.field_width + self.offset_x - goal_width, self.offset_y + goal_top_y, self.field_width + self.offset_x, goal_top_y + self.offset_y + goal_height, fill="white", outline="")

    def draw_players(self):
        # Draw players (red for team 1, blue for team 2)
        for solution in prolog.query("player(ID, Team, Role, position(X, Y), _)"):
            player_id = solution["ID"]
            team = solution["Team"]
            role = solution["Role"]
            x = solution["X"]
            y = solution["Y"]

            cx = self.offset_x + x
            cy = self.offset_y + y

            radius = 10

            color = "red" if team == "team1" else "blue"
            self.canvas.create_oval(cx - radius, cy - radius, cx + radius, cy + radius, fill=color)

            # Display the role text
            self.canvas.create_text(cx, cy - (radius+5), text=(player_id+" "+role), fill="black")

    def draw_ball(self):
        # Get ball position and convert to GUI coordinates
        for solution in prolog.query("ball(position(X, Y))"):
            x = solution["X"]
            y = solution["Y"]

            radius = 8

            cx = self.offset_x + x
            cy = self.offset_y + y
            self.canvas.create_oval(cx - radius, cy - radius, cx + radius, cy + radius, fill="black")


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
