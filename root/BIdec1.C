#include <iostream>
#include <TGraphErrors.h>
#include <TAxis.h>
#include <TCanvas.h>
#include <TF1.h>
#include <iomanip>
#include <cmath>

using namespace std;

void BIdec1(){

double I[]={10.04,9.5,8.97,8.49,7,6.05,4.98,4.02,3,2,1,0};
double B[]={0.636,0.613,0.59,0.566,0.478,0.416,0.348,0.282,0.215,0.145,0.078,0.009};
double sI[12];
double sB[12];

for(int i=0;i<12;i++) {
sI[i]=0.02;
sB[i]=0.001;
}

TCanvas *canvas = new TCanvas("canvas","B(I)",0,0,600,400);
canvas->SetFillColor(0);
canvas->cd();

TGraphErrors *TGE = new TGraphErrors(12,I,B,sI,sB);
TGE->SetMarkerSize(0.6);
TGE->SetMarkerStyle(21);
TGE->SetTitle("B(I)");
TGE->GetXaxis()->SetTitle("I [A]");
TGE->GetYaxis()->SetTitle("B [T]");
TGE->Draw("AP");

TF1 *func = new TF1("func","[0]*x + [1]",0,7);
func->SetParameter(0,0.06);
func->SetParameter(1,0);
func->SetLineColor(2);
func->Draw("same");
TGE->Fit("func","RM+S");

cout << "Chi^2:" << func->GetChisquare() << ", number of DoF: " << func->GetNDF() << " (Probability: " << func->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;


TF1 *func1 = new TF1("func1","[0]*x*x + [1]*x + [2]",7,10.04);
func->SetParLimits(0,0,10);
func->SetParLimits(1,0,10);
func->SetParLimits(2,0,10);
/*
func->SetParameter(0,0);
func->SetParameter(1,0);
func->SetParameter(2,0);
*/
func1->SetLineColor(3);
func1->Draw("same");
TGE->Fit("func1","RM+S");

cout << "Chi^2:" << func1->GetChisquare() << ", number of DoF: " << func1->GetNDF() << " (Probability: " << func1->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;

}
