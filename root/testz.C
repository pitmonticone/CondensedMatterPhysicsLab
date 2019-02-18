#include <iostream>
#include <cmath>
#include <string>
using namespace std;

void testz(){

//retta BIcresc1
/*double ac1 = 6.75*pow(10,-2.);
double sac1 = 3*pow(10,-4.);
double bc1 = 2*pow(10,-3.);
double sbc1 = 1*pow(10,-3.);

double apc1 = -4.6*pow(10,-3.);
double sapc1 =1*pow(10,-4.);
double bpc1 = 1.33*pow(10,-1);
double sbpc1 =  */

double a[]={6.75*pow(10,-2.),6.68*pow(10,-2.),6.71*pow(10,-2.),6.74*pow(10,-2.)};//c1,dc1,dc2 per tutti. c2 da inserire quando funzionerà
double sa[] = {3*pow(10,-4.),3*pow(10,-4.),3*pow(10,-4.),3*pow(10,-4.)};//
double b[] = { 2*pow(10,-3.),7*pow(10,-3.),1.1*pow(10,-2.),9*pow(10,-3.)};
double sb[] ={1*pow(10,-3.),1*pow(10,-3.),1*pow(10,-3.),1*pow(10,-3.)};

double ap[]={-4.6*pow(10,-3.),-4.2*pow(10,-3.),-4.5*pow(10,-3.),-4.5*pow(10,-3.)};
double sap[]={1*pow(10,-4.),4*pow(10,-4.),6*pow(10,-5.),6*pow(10,-4.)};
double bp[]={1.33*pow(10,-1.),1.24*pow(10,-1.),1.29*pow(10,-1.),1.3*pow(10,-1.)};
double sbp[]={2*pow(10,-3.),7*pow(10,-3.),1*pow(10,-3.),1*pow(10,-2.)};
double cp[] ={-2.4*pow(10,-1.),-1.9*pow(10,-1.),-2.01*pow(10,-1.),-2.0*pow(10,-1.)};
double scp[]={1*pow(10,-2.),3*pow(10,-2.),5*pow(10,-3.),4*pow(10,-2.)};

string name[4]={"c1","c2","d1","d2"};
//string var[5]={"a","b","ap","bp","cp"};
cout<<endl;
for(int i = 0;i<3;i++){
  for(int j = i+1;j<4;j++){
    cout<<"z"<<" a "<<name[i]<<" "<<name[j]<<" = "<<(a[i]-a[j])/(sqrt(pow(sa[i],2.)+pow(sa[j],2.)))<<endl;
    cout<<"z"<<" b "<<name[i]<<" "<<name[j]<<" = "<<(b[i]-b[j])/(sqrt(pow(sb[i],2.)+pow(sb[j],2.)))<<endl;
    cout<<"z"<<" ap "<<name[i]<<" "<<name[j]<<" = "<<(ap[i]-ap[j])/(sqrt(pow(sap[i],2.)+pow(sap[j],2.)))<<endl;
    cout<<"z"<<" bp "<<name[i]<<" "<<name[j]<<" = "<<(bp[i]-bp[j])/(sqrt(pow(sbp[i],2.)+pow(sbp[j],2.)))<<endl;
    cout<<"z"<<" cp "<<name[i]<<" "<<name[j]<<" = "<<(cp[i]-cp[j])/(sqrt(pow(scp[i],2.)+pow(scp[j],2.)))<<endl;
  }
}

double am =0;
double amnum=0;
double amden=0;
for(int i=0;i<4;i++){
  amnum += a[i]/pow(sa[i],2.);
  amden += 1/pow(sa[i],2.);
}
am=amnum/amden;
double sam=sqrt(1/amden);
cout<<"a (il coeff ang retta) medio= "<<am<<"+-"<<sam<<endl;


double bm =0;
double bmnum=0;
double bmden=0;
for(int i=0;i<4;i++){
  bmnum += b[i]/pow(sb[i],2.);
  bmden += 1/pow(sb[i],2.);
}
bm=bmnum/bmden;
double sbm=sqrt(1/bmden);
cout<<"b (intercetta retta) medio= "<<bm<<"+-"<<sbm<<endl;

double apm =0;
double apmnum=0;
double apmden=0;
for(int i=0;i<4;i++){
  apmnum += ap[i]/pow(sap[i],2.);
  apmden += 1/pow(sap[i],2.);
}
apm=apmnum/apmden;
double sapm=sqrt(1/apmden);
cout<<"apm (coeff x^2) medio= "<<apm<<"+-"<<sapm<<endl;

double bpm =0;
double bpmnum=0;
double bpmden=0;
for(int i=0;i<4;i++){
  bpmnum += bp[i]/pow(sbp[i],2.);
  bpmden += 1/pow(sbp[i],2.);
}
bpm=bpmnum/bpmden;
double sbpm=sqrt(1/bpmden);
cout<<"bpm (coeff x) medio= "<<bpm<<"+-"<<sbpm<<endl;

double cpm =0;
double cpmnum=0;
double cpmden=0;
for(int i=0;i<4;i++){
  cpmnum += cp[i]/pow(scp[i],2.);
  cpmden += 1/pow(scp[i],2.);
}
cpm=cpmnum/cpmden;
double scpm=sqrt(1/cpmden);
cout<<"cpm (termine noto parabola) medio= "<<cpm<<"+-"<<scpm<<endl;

for(int i=0;i<4;i++){
  cout<<"Z"<<"a"<<name[i]<<" am ="<< (a[i]-am)/(sqrt(pow(sa[i],2.)+pow(sam,2.)))<<endl;
}

for(int i=0;i<4;i++){
  cout<<"Z"<<"b"<<name[i]<<" bm ="<< (b[i]-bm)/(sqrt(pow(sb[i],2.)+pow(sbm,2.)))<<endl;
}

for(int i=0;i<4;i++){
  cout<<"Z"<<"ap"<<name[i]<<" apm ="<< (ap[i]-apm)/(sqrt(pow(sap[i],2.)+pow(sapm,2.)))<<endl;
}

for(int i=0;i<4;i++){
  cout<<"Z"<<"bp"<<name[i]<<" bpm ="<< (bp[i]-bpm)/(sqrt(pow(sbp[i],2.)+pow(sbpm,2.)))<<endl;
}

for(int i=0;i<4;i++){
  cout<<"Z"<<"cp"<<name[i]<<" apm ="<< (cp[i]-cpm)/(sqrt(pow(scp[i],2.)+pow(scpm,2.)))<<endl;
}
//escildiamo dunque b del c1, b del d1, cp del c1

double a2[]={6.75*pow(10,-2.),6.68*pow(10,-2.),6.75*pow(10,-2.),6.75*pow(10,-2.)};//c1,dc1,dc2 per tutti. c2 da inserire quando funzionerà
double sa2[] = {3*pow(10,-4.),3*pow(10,-4.),3*pow(10,-4.),3*pow(10,-4.)};//
double b2[] = {7*pow(10,-3.),9*pow(10,-3.)};
double sb2[] ={1*pow(10,-3.),2*pow(10,-3.)};

double ap2[]={-4.6*pow(10,-3.),-4.2*pow(10,-3.),-4.0*pow(10,-3.),-4.2*pow(10,-3.)};
double sap2[]={1*pow(10,-4.),4*pow(10,-4.),3*pow(10,-4.),3*pow(10,-4.)};
double bp2[]={1.33*pow(10,-1.),1.24*pow(10,-1.),1.20*pow(10,-1.),1.24*pow(10,-1.)};
double sbp2[]={2*pow(10,-3.),7*pow(10,-3.),5*pow(10,-3.),4*pow(10,-3.)};
double cp2[] ={-1.9*pow(10,-1.),-2.01*pow(10,-1.),-2.0*pow(10,-1.)};
double scp2[]={3*pow(10,-2.),5*pow(10,-3.),4*pow(10,-2.)};


cout<<"Le medie finali, calcolate solo tra i valori compatibili con la media generale, sono:" <<endl;

cout<<"a (il coeff ang retta) medio finale = "<<am<<"+-"<<sam<<endl;

double bm2 =0;
double bmnum2=0;
double bmden2=0;
for(int i=0;i<2;i++){
  bmnum2 += b2[i]/pow(sb2[i],2.);
  bmden2 += 1/pow(sb2[i],2.);
}
bm2=bmnum2/bmden2;
double sbm2=sqrt(1/bmden2);
cout<<"b (intercetta retta) medio finale= "<<bm2<<"+-"<<sbm2<<endl;

cout<<"apm (coeff x^2) medio finale= "<<apm<<"+-"<<sapm<<endl;

cout<<"bpm (coeff x) medio= "<<bpm<<"+-"<<sbpm<<endl;

double cpm2 =0;
double cpmnum2=0;
double cpmden2=0;
for(int i=0;i<3;i++){
  cpmnum2 += cp2[i]/pow(scp2[i],2.);
  cpmden2 += 1/pow(scp2[i],2.);
}
cpm2=cpmnum2/cpmden2;
double scpm2=sqrt(1/cpmden2);
cout<<"cpm (termine noto parabola) medio finale= "<<cpm2<<"+-"<<scpm2<<endl;
}
