#include <iostream>
#include <TGraphErrors.h>
#include <TAxis.h>
#include <TCanvas.h>
#include <TF1.h>
#include <iomanip>
#include <cmath>

using namespace std;

void zat2(){

const int nmisure=6;
double Imin[]={5.71,6.73,7.25,7.81,8.70,9.91};
double Imax[]={5.79,6.75,7.30,7.83,8.89,9.99};


double ar[]={0.0672,0.00015};
double br[]={0.0074,0.0009};
double ap[]={-0.00452101,5.085e-05};
double bp[]={0.129709,0.000884};
double cp[]={-0.200692,0.004895};
double dm[]={276.0633923077,364.5211988095,387.4279875,476.2313602941,453.9203164286,369.46945};
double Dm[]={4473.7978697368,4492.995351,4514.1695944444,5552.3559029412,4721.02548125,4485.4394416667};
double sdm[]={48.3702188241,20.7574578916,12.7276052048,20.8456162243,30.968891102,35.2988643417};
double sDm[]={35.4283334611,22.7803969221,32.4796439213,470.4302428112,182.4243711224,16.8996739464};
double Im[nmisure];
double sIm[nmisure];
double B[nmisure];
double sB[nmisure];
double dDm[nmisure];
double sdDm[nmisure];


for(int i = 0;i<nmisure;i++){

Im[i]=(Imax[i]+Imin[i])/2.;

if((Imax[i]-Imin[i])/2. < 0.02)
sIm[i]=0.02;
else{
sIm[i]=(Imax[i]-Imin[i])/2.;
}

if(Im[i]<=7){
B[i]=ar[0]*Im[i]+br[0];
sB[i]=sqrt(pow(Im[i]*ar[1],2)+pow(sIm[i]*ar[0],2)+pow(br[1],2));
cout<<"Im = ("<<Im[i]<<"+-"<<sIm[i]<<") A "<<" -------- "<<"B[Im] = ("<<B[i]<<"+-"<<sB[i]<<") T "<<endl;
}
else{
B[i]=ap[0]*pow(Im[i],2)+bp[0]*Im[i]+cp[0];
sB[i]=sqrt(pow(pow(Im[i],2)*ap[1],2)+pow(((2*Im[i]*ap[0])+bp[0])*sIm[i],2)+pow(Im[i]*bp[1],2)+pow(cp[1],2));
cout<<"Im = ("<<Im[i]<<"+-"<<sIm[i]<<") A "<<" -------- "<<"B[Im] = ("<<B[i]<<"+-"<<sB[i]<<") T "<<endl;
}

dDm[i]=dm[i]/Dm[i];
sdDm[i]=sqrt(pow(sdm[i]/Dm[i],2)+pow((dm[i]*sDm[i])/(pow(Dm[i],2)),2));
cout<<"dDm = ("<<dDm[i]<<"+-"<<sdDm[i]<<")"<<endl;
}



TCanvas *canvas = new TCanvas("canvas","dDm(B)",0,0,600,400);
canvas->SetFillColor(0);
canvas->cd();

TGraphErrors *TGE = new TGraphErrors(nmisure,B,dDm,sB,sdDm);
TGE->SetMarkerSize(0.6);
TGE->SetMarkerStyle(21);
TGE->SetTitle("d/D(B)");
TGE->GetXaxis()->SetTitle("B [T]");
TGE->GetYaxis()->SetTitle("d/D");
TGE->Draw("AP");

TF1 *func = new TF1("func","[0]*x+[1]",0,6.93);
func->SetParameter(0,0.06);
func->SetParameter(1,0);
func->SetLineColor(2);
func->Draw("same");
TGE->Fit("func","RM+S");

cout << "Chi^2:" << func->GetChisquare() << ", number of DoF: " << func->GetNDF() << " (Probability: " << func->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;

}

