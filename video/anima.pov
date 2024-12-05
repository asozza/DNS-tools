// Animation of Lagrangian tracers 

//global_settings { assumed_gamma 2.2 }
global_settings { assumed_gamma 1.2 }

#declare nf=10;
#declare np=10000;

#declare fi=0.5*6.28318*frame_number/nf;
#declare r=12;
#declare cx=r*sin(fi);
#declare cz=r*cos(fi);

//camera {
//  location  <cx, 6, cz>
//  look_at   <0, 0, 0>
//  angle 60
//}

camera {
  location  <0, 6, -12>
  look_at   <0, 0, 0>
  angle 60
}

light_source { <2*cx, 12, 2*cz> color rgb <2,2,2> shadowless}
//light_source { <10, 30, -20> color rgb <1,1,1> shadowless}

//light_source { <-10, 30, -20> color rgb <1,1,1> shadowless}
//light_source { <10, 30, 20> color rgb <1,1,1> shadowless}
//light_source { <-10, 30, -20> color rgb <1,1,1> shadowless}

//plane{<0, 1, 0>, -10 pigment {checker color <1,1,1>, color <0,0,0> scale 4} }

//box {<-pi,-pi,-pi>,<pi,pi,pi>
//pigment {color rgbf <1.0,1.0,1.0,0.95>}}

#declare r=0.02;
cylinder {<-pi,-pi,-pi>, <-pi,-pi,pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<-pi,-pi,-pi>, <-pi,pi,-pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<-pi,-pi,-pi>, <pi,-pi,-pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<pi,pi,pi>, <-pi,pi,pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<pi,pi,pi>, <pi,-pi,pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<pi,pi,pi>, <pi,pi,-pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<-pi,pi,pi>, <-pi,pi,-pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<-pi,pi,pi>, <-pi,-pi,pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<pi,-pi,-pi>, <pi,-pi,pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<pi,-pi,-pi>, <pi,pi,-pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<pi,pi,-pi>, <-pi,pi,-pi>,r
   pigment {color rgb <1,1,1>}}
cylinder {<pi,-pi,pi>, <-pi,-pi,pi>,r
   pigment {color rgb <1,1,1>}}

//background { color rgb <0.1, 0.1, 0.1> }
//fog {distance 200 color rgb <0.1,0.1,0.1>}

// Bounds velocita
//#declare vmin=0.0;
//#declare vmax=6.58;


#declare i=1000+frame_number;
#declare nome=concat("lag.",str(i,4,0));
#fopen fp nome read

#declare i=1;
#while (i<=np)

#read(fp,xp,yp,zp)

// Calcola colore
//#declare velo=sqrt(vx*vx+vy*vy+vz*vz);
//#declare velo=(velo-vmin)/(vmax-vmin);
//#if (velo < 0.5)
//  #declare r=0;
//  #declare g=2.0*velo;
//  #declare b=1.0-2.0*velo;
//#else
//  #declare r=2.0*(velo-0.5);
//  #declare g=1.0-2.0*(velo-0.5);
//  #declare b=0;
//#end

sphere { <xp,yp,zp>, 0.02
  texture {
   pigment {color rgb <0,1,0>}
   finish {phong 1}
  }
}
 
//text {
// ttf "timrom.ttf" "POV-RAY 3.0" 1, 0
// pigment {rgb 1} finish {ambient 1 diffuse 0} no_shadow
//}

#declare i=i+1;
#end

#fclose fp 


