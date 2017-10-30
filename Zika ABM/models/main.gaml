/**
 *  main
 *  Author: naveen3124 & Dattanand
 *  Description: Main Function
 *  Base Predator X Prey
 */

model main
		/* Insert your model definition here */
global {
	
	//Gis Csv and other external data 
	file tmp_csv_file <- csv_file("../includes/TemperatureCsv - Sheet1.csv",",");
	
	//Simulation Global Data 
	float step <- 60 #mn;
	int day <- 1 ;
	int hr_perday <- 0;
	
	//Mobility Data
	file roads_shapefile <- file("../includes/dre.shp");
	file buildings_shapefile <- file("../includes/dreroads.shp");
	file shape_file_bounds <- file("../includes/bounds.shp");
	geometry shape <- envelope(shape_file_bounds);
	//geometry shape <- envelope(roads_shapefile);
	graph road_network;
	
	
	//Species Global Data 
    int nb_mosquitoes_init <- 200;
    int nb_humans_init <- 100;
    int mosquito_no <- 200;
    
    //Human Species Global Data
    int number_S <- 495;
    int number_I <- 5 ;
    int number_R <- 0 ;
    float survivalProbability <- 1/(70*365) ;
	float beta <- 0.05 ; 	// The parameter Beta
	float nu <- 0.001 ;
	float delta <- 0.01 parameter: "Delta (I->R)"; // The parameter Delta
	int numberHumans <- number_S+number_I+number_R;
	bool local_infection <- false parameter: "Is the infection is computed locally?";
	int neighbours_size <- 2 min:1 max: 5 parameter:"Size of the neighbours";
	int nb_infected <- number_I;
	float R0 ;
    
    //Mosquitoes Species Global Data
    float fetal_mortality <- 0.01;
	float mortality_emergence <- 0.02;
	float prob_mating <- 0.2;
	float sex_ratio <- 0.5;	
	
	float mortality_rate <- 0.05; 
	int max_meals <- 2; 
	float prob_reproduce <- 0.01;
	float adult_mortality <- 0.05;
	
    
   //Static Variables 
    float pie <- 3.14159265;
    matrix data;
    
    
    init {
        create mosquitoes number: nb_mosquitoes_init ;
        create humans number: nb_humans_init {
        	is_susceptible <- true;
        	is_infected <-  false;
            is_immune <-  false; 
            color <-  rgb("green");
        }
        create humans number: number_I {
            is_susceptible <-  false;
            is_infected <-  true;
            is_immune <-  false; 
            color <-  rgb("red"); 
       }
         data <- matrix(tmp_csv_file);
         write data.rows  ;
         
         create roads from: roads_shapefile;
		road_network <- as_edge_graph(roads);
		create buildings from: buildings_shapefile;
		create humans number:1000 {
			buildings init_place <- one_of(buildings);
			location <- any_location_in(init_place) + {0,0, init_place.height};
			target <- any_location_in(one_of(buildings));
		}
		//loop on the matrix rows (skip the first header line)
        
    }
    
   reflex compute_nb_infected {
   		nb_infected <- humans count (each.is_infected);
   }     
    reflex global_reflex {
    	hr_perday <- hr_perday + 1;
    	if(hr_perday = 24)
    	{
    		hr_perday <- 0;
    		
				write data[0,day];
				day <- (day + 1) mod (data.rows - 1) ;  	
				if(day = 0)
				{
					day <- 1;
				}
    	}
// statements...
	}
}

species roads {
	aspect geom {
		draw shape color: rgb("black");
	}
}

species buildings {
	float height <- 10.0+ rnd(10);
	int nb_I -> {members count (people_in_building(each).is_infected)};
	int nbInhabitants update: length(members);				
	list<people_in_building> membersS <- [] update: list<people_in_building>(members) where (!each.is_infected);
	list<people_in_building> membersI <- [] update: list<people_in_building>(members) where (each.is_infected);
	float t;    
	float S update: length(membersS) as float; 
   	float I update: length(membersI) as float;
   	float I_to_1 <- 0.0;
   	float h<-0.1;
   	
	aspect geom {
		draw shape color: empty(members) ? rgb("gray") : (nb_I/length(members) > 0.5 ? rgb("red") : rgb("green")) depth: height;
	}
	species people_in_building parent: humans schedules: [] {
		int leaving_time;
		aspect circle{}
	}
	reflex let_people_enter {
		list<humans> entering_people <- (humans inside self);
		if !(empty (entering_people)) {
			capture entering_people as: people_in_building returns: people_captured;
			ask people_captured {
				leaving_time <- int(time + 50 + rnd(50));
			}
 		}
	}
	reflex let_people_leave  {
		list<people_in_building> leaving_people <- list<people_in_building>(members) where (time >= each.leaving_time);
		if !(empty (leaving_people)) {
			release leaving_people as: humans in: world;
		}
	}
			
	equation SIR{ 
		diff(S,t) = (- beta * S * I / nbInhabitants) ;
		diff(I,t) = (  beta * S * I / nbInhabitants) ;
	}

	reflex epidemic when:(S>0 and I>0){ 	
		float I0 <- I;
    	solve SIR method: "rk4" step: h ;
    	I_to_1 <- I_to_1 + (I - I0);
    	if(I_to_1 > 1) {
    		ask(membersS){
    			is_infected <- true;
    			myself.I_to_1 <- myself.I_to_1 - 1;
    		}
    	}
    }    
}

species generic_species skills:[moving] {
	float size <- 1.0;
	rgb color  ;
	float max_energy;
	float max_transfert;
	float energy_consum;
	city_climate_cell myCell <- one_of (city_climate_cell) ;
	float energy <- (rnd(1000) / 1000) * max_energy  update: energy - energy_consum max: max_energy ;
			 point target;
	
	init {
		location <- myCell.location;
	}
	reflex move {
		do goto target:target on: road_network;
		if (location = target) {
			target <- any_location_in(one_of(buildings));
		}
	}
	reflex die when: energy <= 0 {
	//	do die ;
	}
	
	aspect base {
		draw circle(size) color: color ;
	}
}


species egg {
	int age <- 0;
	
	int max_age;
	
	reflex age when: time mod 864=0{
		age <- age + 1;
		if flip(1 - fetal_mortality){
			if(flip(1 - mortality_emergence)){				
			
		if(age>=max_age){
			if flip(sex_ratio){
				create mosquitoes {
						mos_age <- 0;
				}
				mosquito_no <- mosquito_no+1;		
			}
			
			do die;
		}
		}
			
		}
	}
}
species humans parent: generic_species {
	//rgb color <- #blue;
	
	bool is_susceptible <- true;
	bool is_infected <- false;
   	bool is_immune <- false;
   	bool is_exposed <- false;
    int sic_count <- 0;
   
      reflex become_infected when: is_susceptible {
        	float rate  <- 0.0;
        	if(local_infection) {
        		int nb_hosts  <- 0;
        		int nb_hosts_infected  <- 0;
        		loop hst over: ((myCell.neighbours + myCell) accumulate (humans overlapping each)) {
        			nb_hosts <- nb_hosts + 1;
        			if (hst.is_infected) {nb_hosts_infected <- nb_hosts_infected + 1;}
        		}
        		rate <- nb_hosts_infected / nb_hosts;
        	} else {
        		rate <- nb_infected / numberHumans;
        	}
        	if (flip(beta * rate)) {
	        	is_susceptible <-  false;
	            is_infected <-  true;
	            is_immune <-  false;
	            color <-  rgb("red");    
	        }
        }
        
        reflex become_immune when: (is_infected and flip(delta)) {
        	is_susceptible <- false;
        	is_infected <- false;
            is_immune <- true;
            color <- rgb("blue");
        }
        
        reflex shallDie when: flip(nu) {
			create species(self)  {
				myCell <- myself.myCell ;
				location <- myself.location ; 
			}
           	do die;
        }
	
}
	
species mosquitoes parent: generic_species {
	rgb color <- #red ;
	
	int num_meals_today <- 0;
	bool carrying_eggs <- false;
	int time_since_eggs <- 0;
	int mos_age <- 0;
	int adult_lifespan <- 5;
	reflex age when: time mod 864=0{
		mos_age <- mos_age + 1;
		if(mos_age > adult_lifespan){				
			do die;
			mosquito_no <- mosquito_no-1;
		}
	}
	
	
	reflex feed when:  hr_perday>=18 and hr_perday<=21 and time mod 6 = 0 and num_meals_today< max_meals{
		
		
					num_meals_today <- num_meals_today + 1;
					
					
	}
	
	
	reflex reproduce when: time_since_eggs>=3 and time mod 864 = 0{
			
			create egg number:10{
				
				age <- 0;
				max_age <- 5;
			}
		
		time_since_eggs <- 0;
		carrying_eggs <- false;
		
		
		
	}
	
	reflex nextDay when: time mod 864 = 0{
		
		if flip(adult_mortality){
			mosquito_no <- mosquito_no-1;
			do die;
		}
		num_meals_today <- 0;
		if(carrying_eggs){
			time_since_eggs <- time_since_eggs+1;
		}		
		if(!carrying_eggs){
			if flip(prob_mating){
				if flip(sex_ratio){
				carrying_eggs <- true;
				}
			}
		}
	}
}

grid city_climate_cell width: 50 height: 50 neighbours: 8 {
    float maxTemp <- 40.0 ;
    float maxPrecipitation <-200.0 ;
    //need to convert to celsius and divide into rainfall , summer and winter
    float climateChange <- (rnd(1000) / 1000) * 0.01 ;
//    float currentClimate <- (rnd(1000) / 1000) max: maxTemp update: currentClimate + climateChange ;
	float currentClimate <- 30.5 max: maxTemp;
	float currentPrecipitation <- 0.0 max: maxPrecipitation; 
  //  rgb color <- rgb(int(255 * (1 - currentClimate)), 255, int(255 * (1 - currentClimate))) update: rgb(int(255 * (1 - currentClimate)), 255, int(255 *(1 - currentClimate))) ;
  	rgb color <- rgb(int(255), 255, 0) ;
  	list<city_climate_cell> neighbours  <- (self neighbours_at neighbours_size); 
  	reflex per_cell
  	{
  		currentClimate <- float(data[1,day]);
  		//write currentClimate;
  		currentPrecipitation <- float(data[3,day]);
  		color <- rgb(int(255 * (currentClimate/maxTemp)), int(255 * (currentClimate/maxTemp)), int(255 * (currentPrecipitation /maxPrecipitation))) ;
	
  	}
}

experiment Mosquitoes_Human_Zika type: gui {
    parameter "Initial number of mosquitoes: " var: nb_mosquitoes_init min: 1 max: 1000 category: "Mosquitoes" ;
   	parameter "Initial number of humans: " var: nb_humans_init min: 1 max: 1000 category: "Humans" ;
   	
   	parameter "Number of Susceptible" var: number_S ;// The number of susceptible
    parameter "Number of Infected" var: number_I ;	// The number of infected
    parameter "Number of Removed" var:number_R ;	// The number of removed
 	parameter "Survival Probability" var: survivalProbability ; // The survival probability
	parameter "Beta (S->I)" var:beta; 	// The parameter Beta
	parameter "Mortality" var:nu ;	// The parameter Nu
	parameter "Delta (I->R)" var: delta; // The parameter Delta
	parameter "Is the infection is computed locally?" var:local_infection ;
	parameter "Size of the neighbours" var:neighbours_size ;
    
    output {
        display main_display {
            grid city_climate_cell lines: #black ;
        	species mosquitoes aspect: base ;
        	         	species humans aspect: base;
        	
        	
			species buildings aspect:geom;
			species roads aspect:geom;
        }
        display chart refresh_every: 10 {
			
			chart "Susceptible and Mosquitoes Population" type: series background: rgb("lightGray") style: exploded {
				data "susceptible" value: humans count (each.is_susceptible) color: rgb("green");
				data "infected" value: humans count (each.is_infected) color: rgb("red");
				data "immune" value: humans count (each.is_immune) color: rgb("blue");
				data "Mosquito_Population" value: mosquito_no color: #blue;
			}
			}
		
        
    }
}