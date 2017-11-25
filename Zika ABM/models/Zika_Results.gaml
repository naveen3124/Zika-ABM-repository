/**
 *  main
 *  Author: naveen3124 & Dattanand
 *  Description: Main Function
 */

model Zika_Results
		/* Insert your model definition here */
global {
	
	//Gis Csv and other external data 
	file tmp_csv_file <- csv_file("../models/captured_data.csv",",");
	
	//Simulation Global Data 
	float step <- 60 #mn;
	
    matrix data;
    int rx1 <- 0;
    int ry1 <- 0;
    int rx2 <- 400;
    int ry2 ;
    int rx3 ;
    int ry3 ;
    int ix1 <- 0;
    int iy1 <- 0;
    int ix2 <- 400;
    int iy2 ;
    int ix3 ;
    int iy3 ;
    int lx1 <- 0;
    int ly1 <- 0;
    int lx2 <- 400;
    int ly2 ;
    int lx3 ;
    int ly3 ;
    init {
       
         data <- matrix(tmp_csv_file);
        int avgrows <- data.rows ;
        write avgrows;
        int cnt <- 0;
         loop times: avgrows
         {
         	int tmp <- data[1,cnt];
         	ry2 <- ry2 + tmp;
         	tmp <- data[2,cnt];
         	iy2 <- iy2 + tmp;
         	tmp <- data[3,cnt];
         	ly2 <- ly2 + tmp;
         	cnt <- cnt + 1;
         }
       
        ry2<- ry2/avgrows;
        iy2<- iy2/avgrows;
       	ly2<- ly2/avgrows;
        write ry2  ;
  		write iy2 ;
  		write ly2; 
       	rx3 <- rx2 + 100;
       	ix3 <- ix2 + 100;
       	lx3 <- lx2 + 100;
       	
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
			ly3 <- ly1 + (((lx3 - lx1)/ (lx2 - lx1))* (ly2 - ly1));
    		lx1 <- lx2;
    		ly1 <- ly2 ;
			lx2 <- lx3 ;
			ly2 <- ly3 ;
			lx3	<- lx3 + 100;
			
		}
		write rx3;
		write " "; 
		write ry3 ;
		write iy3 ;
		write ly3 ;
		do pause ; 
	}
   
}



experiment Results type: gui {
   
    output {
        display main_display {
          
        }
        display chart refresh_every: 10 {
			chart " Zika Virus Epidemic in Different regions of City" type: pie style: exploded size: {1, 0.5} position: {0, 0.5}{
				data "Residential" value: ry3 color: #magenta ;
				data "Industrial" value: iy3 color: #blue ;
				data "Leisure" value: ly3 color: #red ;
				}
			}
		}
		
}