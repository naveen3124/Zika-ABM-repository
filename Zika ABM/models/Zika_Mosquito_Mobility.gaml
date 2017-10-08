/**
 *  Zika_Mobility_Transmission
 *  Author: naveen3124 and Dattanand A
 *  Description: Mobility Of Human Agents in City
 */

model Zika_Mobility_Transmission

global {
	
	//Simulation Timings
	float step <- 10 #mn;
	int current_hour update: (time / #hour) mod 24;
	int days_passed update: int(time/86400);
	int current_month update: int(days_passed/30);
	bool is_night <- true update: current_hour < 7 or current_hour > 20;	
	
	//Load GisData and Temperature Csv File
	file tmp_csv_file <- csv_file("../includes/TemperatureCsv - Sheet1.csv",",");
	file shape_file_buildings <- file("../includes/RhBuildings.shp");
	file shape_file_roads <- file("../includes/Rhroads.shp");
	file shape_file_bounds <- file("../includes/Rhbounds.shp");
	geometry shape <- envelope(shape_file_bounds);
	graph the_graph;
	
	//Global Variables
	int nb_people <- 200;
	int mosquito_no <- 600;
	int nb_infected_init <- 1;
	
	//Mosquito Disease Parameters Global
	float fetal_mortality <- 0.01;
	float mortality_emergence <- 0.02;
	float prob_mating <- 0.2;
	float sex_ratio <- 0.5;	
	float mortality_rate <- 0.05; 
	int max_meals <- 2; 
	float prob_reproduce <- 0.01;
	float adult_mortality <- 0.05;
	float sensor_range <- 4.0 #m;
	
	//Human Disease Parameters Global
	float human_sensor_range <- 1.0 #m;
	
	//Human Mobilty Parameters
	int min_work_start <- 6;
	int max_work_start <- 8;
	int min_work_end <- 16; 
	int max_work_end <- 20; 
	float min_speed <- 1.0 #km / #h;
	float max_speed <- 5.0 #km / #h; 
	float destroy <- 0.02;
	int repair_time <- 2 ;
	
	//Validation Parameters
	int nb_people_infected <- nb_infected_init update: people count (each.is_infected);
	int nb_people_not_infected <- nb_people - nb_infected_init update: nb_people - nb_people_infected;
	float infected_rate update: nb_people_infected/nb_people;
	int nb_mosquito_infected <- 1 update: mosquito count (each.is_infected);
	int nb_egg_infected <- 0 update: egg count (each.is_infected);
	int nb_infected_cumulative <- nb_infected_init;
	int nb_eggs <- 0 update: egg count(each.age>=0);
	
	
	
	
	
	init {
		create building from: shape_file_buildings with: [type::string(read ("NATURE"))] {
			if type="Industrial" {
				color <- #red ;
			}
			if type="Leisure" {
				color <- #pink ;
			}
			if type="Lake" {
				color <- #blue ;
			}
		}
		create road from: shape_file_roads ;
		map<road,float> weights_map <- road as_map (each:: (each.destruction_coeff * each.shape.perimeter));
		the_graph <- as_edge_graph(road) with_weights weights_map;
		
		
		list<building> residential_buildings <- building where (each.type="Residential");
		list<building>  industrial_buildings <- building  where (each.type="Industrial") ;
		list<building>  watersources <- building  where (each.type="Lake") ;
		
		create people number: nb_people {
			speed <- min_speed + rnd (max_speed - min_speed) ;
			start_work <- min_work_start + rnd (max_work_start - min_work_start) ;
			end_work <- min_work_end + rnd (max_work_end - min_work_end) ;
			living_place <- one_of(residential_buildings) ;
			working_place <- one_of(industrial_buildings) ;
			objective <- "resting";
			location <- any_location_in (living_place); 
		}
		
		create egg number:25{
				living_place <- one_of(watersources) ;
				is_infected <- false;
				max_age <- 8;
				age <- 0;
			}
		create mosquito  number:mosquito_no {
				living_place <- one_of(watersources);
				location <- any_location_in(living_place);	
				mos_age <- 0;
		}
	}
	
	reflex update_graph{
		map<road,float> weights_map <- road as_map (each:: (each.destruction_coeff * each.shape.perimeter));
		the_graph <- the_graph with_weights weights_map;
	}
	reflex repair_road when: every(repair_time #hour / step) {
		road the_road_to_repair <- road with_max_of (each.destruction_coeff) ;
		ask the_road_to_repair {
			destruction_coeff <- 1.0 ;
		}
	}
}

species egg {
	int age <- 0;
	bool is_infected <- false;
	int max_age;
	building living_place <- nil ;
	
	reflex age when: time mod 86400=0{
		age <- age + 1;
		if flip(1 - fetal_mortality){
			if(flip(1 - mortality_emergence)){
					if(age>=max_age){
						if flip(sex_ratio){
							create mosquito {
								living_place <- myself.living_place ;
								location <- any_location_in(living_place);
								is_infected <- is_infected;
								mos_age <- 0;
							}
							mosquito_no <- mosquito_no+1;
							do die;
						}
					}
				}
			}
		}
}

species mosquito skills:[moving]{	
	float speed <- (0.1 + rnd(1.0)) #km/#h;
	bool is_infected <- false;
	building living_place <- nil ;
	int time_since_virus <- 0;
	int time_to_mature <- 3 ;	
	
	int num_meals_today <- 0;
	bool carrying_eggs <- false;
	int time_since_eggs <- 0;
	int mos_age <- 0;
	int adult_lifespan <- 5;
	int time_passed_virus <- 0;
	
	reflex move when:  !(current_hour < 7 or current_hour > 20){
		do wander amplitude:350 #m;
	}

	reflex age when: time mod 86400=0{
		mos_age <- mos_age + 1;
		if(mos_age > adult_lifespan){				
			do die;
			mosquito_no <- mosquito_no-1;
		}
	}
	
	reflex feed when:  current_hour>=9 and current_hour<=18 and time mod 600 = 0 and num_meals_today<max_meals{
		if is_infected{
			ask any (people at_distance sensor_range) {
					myself.num_meals_today <- myself.num_meals_today + 1;
					if myself.is_infected{
						float p_trans <- 0.6;
						if (state=0){
							if flip(p_trans){	
								is_infected <- true;
								state <- 1;
								nb_infected_cumulative <- nb_infected_cumulative+1;
							}
						}
					}
			}
		}
		else {
			ask any (people at_distance sensor_range) {
					myself.num_meals_today <- myself.num_meals_today + 1;
					if is_infected{
						float p_trans <- 0.5;
						if flip(p_trans) {
							myself.is_infected <- true;
						}
					}
				
			}
		}
	}
	
	
	reflex reproduce when: time_since_eggs>=3 and time mod 600 = 0{
			create egg number:10{
				living_place <- myself.living_place ;
				age <- 0;
				max_age <- 8;
			}		
		time_since_eggs <- 0;
		carrying_eggs <- false;
	}
	reflex nextDay when: time mod 86400 = 0{
		
		if flip(adult_mortality){
			mosquito_no <- mosquito_no-1;
			do die;
		}
		num_meals_today <- 0;
		if(carrying_eggs){
			time_since_eggs <- time_since_eggs+1;
		}		
		if(is_infected){
			time_passed_virus <- time_passed_virus;
		}
		if(!carrying_eggs){
			if flip(prob_mating){
				if flip(sex_ratio){
				carrying_eggs <- true;
				}
			}
		}
	
	
}
aspect base{
		draw shape color:is_infected ? #violet : #green;
	}
}

species building {
	string type; 
	rgb color <- #gray  ;
	
	aspect base {
		draw shape color: color ;
	}
}

species road  {
	float destruction_coeff <- 1 + ((rnd(100))/ 100.0) max: 2.0;
	int colorValue <- int(255*(destruction_coeff - 1)) update: int(255*(destruction_coeff - 1));
	rgb color <- rgb(min([255, colorValue]),max ([0, 255 - colorValue]),0)  update: rgb(min([255, colorValue]),max ([0, 255 - colorValue]),0) ;
	
	aspect base {
		draw shape color: color ;
	}
}

species people skills:[moving] {
	rgb color <- #black ;
	
	//mobility Parameters
	building living_place <- nil ;
	building working_place <- nil ;
	int start_work ;
	int end_work  ;
	string objective ; 
	point the_target <- nil ;
	
	//Disease Parameters
	bool is_infected <- false;	
	bool in_my_house <- true;
	int state <- 0; // 0->Susceptible 1->Exposed 2->Infected 3-> Cured
	list<int> state_duration <- [0,0,0,0]; 
	int days_infected <- 0;
	
	reflex infect when:  state = 2{
			ask any (people at_distance human_sensor_range) {
					if self.state = 1{
						float p_trans <- 0.2;
						
							if flip(p_trans){	
								is_infected <- true;
								state <- 2;
								nb_infected_cumulative <- nb_infected_cumulative+1;
							}
					}
			}
		}	
	
	reflex time_to_work when: current_hour = start_work and objective = "resting"{
		objective <- "working" ;
		the_target <- any_location_in (working_place);
		in_my_house <- false;
	}
		
	reflex time_to_go_home when: current_hour = end_work and objective = "working"{
		objective <- "resting" ;
		the_target <- any_location_in (living_place);
		in_my_house <- true;
		 
	} 
	 
	reflex move when: the_target != nil {
		path path_followed <- self goto [target::the_target, on::the_graph, return_path:: true];
		list<geometry> segments <- path_followed.segments;
		loop line over: segments {
			float dist <- line.perimeter;
			ask road(path_followed agent_from_geometry line) { 
				destruction_coeff <- destruction_coeff + (destroy * dist / shape.perimeter);
			}
		}
		if the_target = location {
			the_target <- nil ;
		}
	}
	reflex health when: (state=1 or state=2) and (time mod 86400 = 0){		
			days_infected <- days_infected+1;
		if state = 1 and (days_infected - state_duration[1] = 0){
				state <- 2;
		}
		else if state = 2 and (days_infected - state_duration[1] - state_duration[2] = 0){				
					state <- 3;				
				is_infected <- false;
		}	
	}
	aspect base {
		draw shape color: color;
	}
}

experiment Human_intercity_mobility type: gui {
	parameter "Shapefile for the buildings:" var: shape_file_buildings category: "GIS" ;
	parameter "Shapefile for the roads:" var: shape_file_roads category: "GIS" ;
	parameter "Shapefile for the bounds:" var: shape_file_bounds category: "GIS" ;
	parameter "Number of people agents" var: nb_people category: "People" ;
	parameter "Earliest hour to start work" var: min_work_start category: "People" min: 2 max: 8;
	parameter "Latest hour to start work" var: max_work_start category: "People" min: 8 max: 12;
	parameter "Earliest hour to end work" var: min_work_end category: "People" min: 12 max: 16;
	parameter "Latest hour to end work" var: max_work_end category: "People" min: 16 max: 23;
	parameter "minimal speed" var: min_speed category: "People" min: 0.1 #km/#h ;
	parameter "maximal speed" var: max_speed category: "People" max: 10 #km/#h;
	
	output {
		monitor "Current Day" value: days_passed;
	monitor "Current Hour" value: current_hour;
	monitor "Current Month" value: current_month;
	monitor "Current Time" value: time/60;
	monitor "Infected people" value: nb_people_infected; 
	monitor "Infected mosqutios" value: nb_mosquito_infected;		
	monitor "Infected people cumulative" value: nb_infected_cumulative;	
		display city_display type:opengl {
			species building aspect: base ;
			species road aspect: base ;
			species people aspect: base ;
			species mosquito aspect: base; 
		}
		display chart_display refresh_every: 10 { 
			chart "Epidemic Status" type: series size: {1, 0.5} position: {0, 0} {
				data "Epidemic in Residential" value: mean (road collect each.destruction_coeff) style: line color: #green ;
				data "Epidemic in Industrial" value: road max_of each.destruction_coeff style: line color: #red ;
				data "Epidemic in Leisure" value: road max_of each.destruction_coeff style: line color: #black ;
				
			}
			chart "People Objectif" type: pie style: exploded size: {1, 0.5} position: {0, 0.5}{
				data "Working" value: people count (each.objective="working") color: #magenta ;
				data "Resting" value: people count (each.objective="resting") color: #blue ;
			}
		}
		display chart1 refresh_every: 10 {
			chart "Disease spread" type: series {
				data "infected" value: nb_people_infected color: #red;
				data "infected cumulative (/500)" value: nb_infected_cumulative color: #blue;
			}
		}
		
		display chart2 refresh_every: 10 {
			chart "Mosquito Population" type: series {
				data "Mosquito_Population" value: mosquito_no color: #blue;
				data "infected" value: nb_mosquito_infected color: #red;
				data "num_eggs" value: nb_eggs color: #black;
			}
		}
	}
}