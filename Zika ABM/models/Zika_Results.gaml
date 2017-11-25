/**
 *  main
 *  Author: naveen3124 & Dattanand
 *  Description: Main Function
 */

model Zika_Results
		/* Insert your model definition here */
global {
	
	//Gis Csv and other external data 
	file tmp_csv_file <- csv_file("../models/Totalcaptured_data360_delhi.csv",",");
	
	//Simulation Global Data 
	float step <- 60 #mn;
	int pop <- 1000;
    matrix data;
    float rx1 <- 0;
    float ry1 <- 0;
    float rx2 <- pop;
    float ry2 ;
    float rx3 ;
    float ry3 ;
    float ix1 <- 0;
    float iy1 <- 0;
    float ix2 <- pop;
    float iy2 ;
    float ix3 ;
    float iy3 ;
    float lx1 <- 0;
    float ly1 <- 0;
    float lx2 <- pop;
    float ly2 ;
    float lx3 ;
    float ly3 ;
    float inx1 <- 0;
    float iny1 <- 0;
    float inx2 <- pop;
    float iny2 ;
    float inx3 ;
    float iny3 ;
    float tny3;
    init {
       
         data <- matrix(tmp_csv_file);
        int avgrows <- data.rows ;
        int cnt <- 0;
         loop times: avgrows
         {
         	int tmp <- data[3,cnt];
         	ry2 <- ry2 + tmp;
         	
         	tmp <- data[4,cnt];
         	iy2 <- iy2 + tmp;
         	tmp <- data[5,cnt];
         	ly2 <- ly2 + tmp;
         	
         	tmp <- data[0,cnt];
         	iny2 <- iny2 + tmp;
         	cnt <- cnt + 1;
         }
        ry2<- ry2/avgrows;
        iy2<- iy2/avgrows;
       	ly2<- ly2/avgrows;
       	iny2<- iny2/avgrows;
        write ry2  ;
  		write iy2 ;
  		write ly2; 
  		write iny2;
       	rx3 <- rx2 + 100;
       	ix3 <- ix2 + 100;
       	lx3 <- lx2 + 100;
     	inx3 <- inx3 + 100;
     }
    reflex linear_extrapolate{
    	loop times: 10000 {
    		ry3 <- ry1 + (((rx3 - rx1)/ (rx2 - rx1))* (ry2 - ry1));
    		rx1 <- rx2;
    		ry1 <- ry2 ;
			rx2 <- rx3 ;
			ry2 <- ry3 ;
			rx3	<- rx3 + 100;
			iy3 <- iy1 + (((ix3 - ix1)/ (ix2 - ix1))* (iy2 - iy1));
    		ix1 <- ix2;
    		iy1 <- iy2 ;
			ix2 <- ix3 ;
			iy2 <- iy3 ;
			ix3	<- ix3 + 100;
			write iy3 ;
			ly3 <- ly1 + (((lx3 - lx1)/ (lx2 - lx1))* (ly2 - ly1));
    		lx1 <- lx2;
    		ly1 <- ly2 ;
			lx2 <- lx3 ;
			ly2 <- ly3 ;
			lx3	<- lx3 + 100;
			iny3 <- iny1 + (((inx3 - inx1)/ (inx2 - inx1))* (iny2 - iny1));
    		inx1 <- inx2;
    		iny1 <- iny2 ;
			inx2 <- inx3 ;
			iny2 <- iny3 ;
			inx3 <- inx3 + 100;
			
		}
		tny3 <- 1000000 - iny3;
		
		write ry3 ;
		
		write ix3 ;
		
		write iy3 ;
		write ly3 ;
		write iny3 ;
		do pause ; 
	}
   
}



experiment Results type: gui {
   
    output {

        display chart refresh_every: 10 {
			chart " Zika Virus Epidemic in Different regions of City" type: pie style: exploded size: {1, 0.5} position: {0, 0.5}{
				data "Residential" value: ry3 color: #magenta ;
				data "Industrial" value: iy3 color: #blue ;
				data "Leisure" value: ly3 color: #red ;
				}
			}
		}
	  	output {

      
		display chart refresh_every: 10 {
			chart " Zika Virus Epidemic Proportion to 1000000 population City Bangalore" type: pie style: exploded size: {1, 0.5} position: {0, 0.5}{
				data "Infected" value: iny3 color: #blue ;
				data "nonInfected" value: tny3 color: #red ;
				}
			
		}
		
		}
	
		
}