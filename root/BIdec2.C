#include <iostream>
#include <TGraphErrors.h>
#include <TAxis.h>
#include <TCanvas.h>
#include <TF1.h>
#include <iomanip>
#include <cmath>

using namespace std;

void BIdec2(){

double I[]={10.06,9.57,9,8.52,8.03,6.93,6.02,5.03,3.96,3.06,2.06,1.09,0};
double B[]={ 0.637,0.618,0.592,0.567,0.542,0.474,0.414,0.35,0.277,0.216,0.15,0.081,0.008};
double sI[13];
double sB[13];

for(int i=0;i<13;i++) {
sI[i]=0.02;
sB[i]=0.001;
}

TCanvas *canvas = new TCanvas("canvas","B(I)",0,0,600,400);
canvas->SetFillColor(0);
canvas->cd();

TGraphErrors *TGE = new TGraphErrors(13,I,B,sI,sB);
TGE->SetMarkerSize(0.6);
TGE->SetMarkerStyle(21);
TGE->SetTitle("B(I)");
TGE->GetXaxis()->SetTitle("I [A]");
TGE->GetYaxis()->SetTitle("B [T]");
TGE->Draw("AP");

TF1 *func = new TF1("func","[0]*x + [1]",0,6.93);
func->SetParameter(0,0.06);
func->SetParameter(1,0);
func->SetLineColor(2);
func->Draw("same");
TGE->Fit("func","RM+S");

cout << "Chi^2:" << func->GetChisquare() << ", number of DoF: " << func->GetNDF() << " (Probability: " << func->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;


TF1 *func1 = new TF1("func1","[0]*x*x + [1]*x + [2]",6.93,10.06);
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
