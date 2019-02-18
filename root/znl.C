#include <iostream>
#include <TGraphErrors.h>
#include <TAxis.h>
#include <TCanvas.h>
#include <TF1.h>
#include <iomanip>
#include <cmath>
using namespace std;

void znl(){
const int nmisure=8;
double Imin[]={2.67,4.82,5.89,6.9,7.22,7.85,8.82,9.82};
double Imax[] = {2.7,4.86,5.92,6.96,7.23,7.86,8.83,9.78};
double ar[]={0.0672,0.00015};
double br[]={0.0074,0.0009};
double ap[]={-0.00452101,5.085e-05};
double bp[]={0.129709,0.000884};
double cp[]={-0.200692,0.004895};
double Im[nmisure];
double sIm[nmisure];
double B[nmisure];
double sB[nmisure];
double dDm[]={0.070,0.14,0.165,0.192,0.202,0.218,0.237,0.263};
double sdDm[]={0.004,0.005,0.005,0.006,0.018,0.006,0.012,0.018};


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

TF1 *func = new TF1("func","[0]*x + [1]",0,6.93);
func->SetParameter(0,0.06);
func->SetParameter(1,0);
func->SetLineColor(2);
func->Draw("same");
TGE->Fit("func","RM+S");

cout << "Chi^2:" << func->GetChisquare() << ", number of DoF: " << func->GetNDF() << " (Probability: " << func->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;

}

