/**
 *  main
 *  Author: naveen3124
 *  Description: Main Function
 *  Base Predator X Prey
 */

model main
		/* Insert your model definition here */
global {
    int nb_mosquitoes_init <- 200;
    init {
        create mosquitoes number: nb_mosquitoes_init ;
    }
}

species mosquitoes {
    float size <- 1.0 ;
    rgb color <- #blue;
    city_climate_cell myCell <- one_of (city_climate_cell) ;

    init {
        location <- myCell.location;
    }

    aspect base {
        draw circle(size) color: color ;
    }
}

grid city_climate_cell width: 100 height: 100 neighbours: 8 {
    float maxTemp <- 1.0 ;
    //need to convert to celsius and divide into rainfall , summer and winter
    float climateChange <- (rnd(1000) / 1000) * 0.01 ;
    float currentClimate <- (rnd(1000) / 1000) max: maxTemp update: currentClimate + climateChange ;
    rgb color <- rgb(int(255 * (1 - currentClimate)), 255, int(255 * (1 - currentClimate))) update: rgb(int(255 * (1 - currentClimate)), 255, int(255 *(1 - currentClimate))) ;
}

experiment Mosquitoes_Human_Zika type: gui {
    parameter "Initial number of mosquitoes: " var: nb_mosquitoes_init min: 1 max: 1000 category: "Mosquitoes" ;
    output {
        display main_display {
            grid city_climate_cell lines: #black ;
            species mosquitoes aspect: base ;
        }
    }
}