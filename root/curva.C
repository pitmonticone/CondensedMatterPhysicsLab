#include <iostream>
#include <TGraphErrors.h>
#include <TAxis.h>
#include <TCanvas.h>
#include <TF1.h>
#include <iomanip>
#include <cmath>

using namespace std;

void curva(){
double ar=0.0672;
double br=0.0074;
double ap=-0.00452101;
double bp=0.129709;
double cp=-0.200692;

double I[]={10.06,0};
double B[]={0.637,0};

TCanvas *canvas = new TCanvas("canvas","B(I)",0,1,600,400);
canvas->SetFillColor(0);
canvas->cd();

TGraphErrors *TGE = new TGraphErrors(2,I,B);
TGE->SetMarkerSize(0);
TGE->SetMarkerStyle(21);
TGE->SetTitle("B(I)");
TGE->GetXaxis()->SetTitle("I [A]");
TGE->GetYaxis()->SetTitle("B [T]");
TGE->Draw("AP");

TF1 *func = new TF1("func","0.0672*x+0.0074",0,7);
func->SetLineColor(2);
func->Draw("same");


TF1 *func1 = new TF1("func1","-0.00452101*pow(x,2)+0.129709*x-0.200692",7,10);
func1->SetLineColor(3);
func1->Draw("same");

}


