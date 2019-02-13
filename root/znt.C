#include <iostream>
#include <TGraphErrors.h>
#include <TAxis.h>
#include <TCanvas.h>
#include <TF1.h>
#include <iomanip>
#include <cmath>

using namespace std;

void znt(){

const int nmisure=8;
const double a = 0.0672 ;
const double sa = 0.0002 ;
const double b = 0.0074 ;
const double sb = 0.0009 ;
const double ap = -0.00452 ;
const double sap = -0.00005;
const double bp = 0.1297 ;
const double sbp = 0.0009;
const double cp = -0.201 ;
const double scp = 0.005 ;
double Imin[]={5.82,6.83,7.90,8.90,9.91,7.20,4.91,2.60,0.00};
double Imax[]={5.85,6.83,7.93,8.92,9.93,7.25,4.93,2.62,0.00};
double Im[nmisure];
double sIm[nmisure];
double B[nmisure];
double sB[nmisure];
cout<<endl;
for(int i=0;i<8;i++){
  Im[i]=(Imin[i]+Imax[i])/2. ;
  if(((Imax[i]-Imin[i])/2.) >= 0.02){
    sIm[i]=(Imax[i]-Imin[i])/2. ;
  }else{
    sIm[i]=0.02;
  }
  B[i]=a*Im[i]+b;
  sB[i]=sqrt(pow(Im[i]*sa,2.)+pow(a*sIm[i],2.)+pow(sb,2.));
  cout<<"Im["<<i<<"] = "<<Im[i]<<" +- "<<sIm[i]<<endl<<"B["<<i<<"] = "<<B[i]<<" +- "<<sB[i]<<endl<<endl;
}

/*TCanvas *canvas = new TCanvas("canvas","B(I)",0,0,600,400);
canvas->SetFillColor(0);
canvas->cd();

TGraphErrors *TGE = new TGraphErrors(nmisure,I,B,sI,sB);
TGE->SetMarkerSize(0.6);
TGE->SetMarkerStyle(21);
TGE->SetTitle("B(I)");
TGE->GetXaxis()->SetTitle("I(A)");
TGE->GetYaxis()->SetTitle("B(T)");
TGE->Draw("AP");

TF1 *func = new TF1("func","[0]*x+[1]",0,7);
func->SetParNames("a","b");
func->SetParameter(0,0.06);
func ->SetParameter(1,0);
func->SetLineColor(2);
func->Draw("same");
TGE->Fit("func","RM+");

cout << "Chi^2:" << func->GetChisquare() << ", number of DoF: " << func->GetNDF() << " (Probability: " << func->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;

TF1 *func1 = new TF1("func1","[0]*pow(x,2.)+[1]*x+[2]",7,10.04);
func1->SetParNames("ap","bp","cp");
func1->SetParameter(0,0.06);
func1 ->SetParameter(1,0);
func1->SetLineColor(3);
func1->Draw("same");
TGE->Fit("func1","RM+");

cout << "Chi^2:" << func1->GetChisquare() << ", number of DoF: " << func1->GetNDF() << " (Probability: " << func1->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;

par0 = func->GetParameter(0);
errpar0 = func->GetParError(0);
cout<<"Il coefficiente angolare, da confrontare con varp, è: "<<par0<<"+-"<<errpar0<<endl<<"varp = "<<varp<<"+-"<<errvarp<<endl<<"Test zeta tra il parametro zero e varp: z= "<<abs(par0-varp)/sqrt(pow(errpar0,2.)+pow(errvarp,2.))<<endl;
for(int i =0; i<7;i++){
  cout<<varr[i]<<" ";
}
cout<<endl;
for(int i =0; i<7;i++){
  cout<<errvarr[i]<<" ";
}*/
}
