nn = 10;
X = array(c(T,F),dim=c(1000)) #������ ��� ����������� ��������
MedX = array(c(T,F),dim=c(1000)) #������ ��� ���������� ������� 
ZR = array(c(T,F),dim=c(1000)) #������ ��� ��������� ������������� ���������� ���������
ZQ = array(c(T,F),dim=c(1000)) #������ ��� ��������� ���������
ZTr = array(c(T,F),dim=c(1000)) #������ ��� ���������� ��������
for(j in 1:1000){
  selection <- rcauchy(n = nn, location=0, scale=1) #������������� ����
  
  selection = sort(selection)
  
  #���������� �������
  s = 0
  s = sum(selection)
  X[j] = s/nn
  
  #���������� ������� (n - ������ ������)
  MedX[j] = (selection[nn / 2] + selection[nn / 2 + 1]) / 2
  
  #��������� ������������� ���������� ���������:
  a = min(selection);
  b = max(selection);
  ZR[j] = (a + b) / 2
  
  #��������� ���������
  m = nn %% 4 #mod(n, 4);
  if (m == 0){
    i = nn / 4;
  }
  if( m != 0){
    i = floor(nn / 4) + 1;
  }
  z1 = selection[i];
  z2 = selection[nn - i + 1];
  ZQ[j] = (z1 + z2)/2
  
  #��������� �������
  r = nn / 4;
  koef = 1 / (nn - 2 * r);
  s = 0;
  arr = array(c(T,F),dim=c(nn - 2*r))
  for (i in floor(r+1):ceiling(nn-r)){
    arr[i - r] = selection[i];
  }
  s = sum(arr)
  ZTr[j] = koef * s
}
XX = sum(X) / 1000  
medx = sum(MedX) / 1000
zr = sum(ZR) / 1000
zq = sum(ZQ) / 1000
ztr = sum(ZTr) / 1000


#���������� ���������

DX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DX[i] = (X[i] - XX) *  (X[i] - XX)
}
dx = sum(DX) / 1000

DMedX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DMedX[i] = (MedX[i] - medx) *  (MedX[i] - medx)
}
dmedx = sum(DMedX) / 1000


DZR = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZR[i] = (ZR[i] - zr) *  (ZR[i] - zr)
}
dzr = sum(DZR) / 1000


DZQ = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZQ[i] = (ZQ[i] - zq) *  (ZQ[i] - zq)
}
dzq = sum(DZQ) / 1000


DZTr = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZTr[i] = (ZTr[i] - ztr) *  (ZTr[i] - ztr)
}
dztr = sum(DZTr) / 1000

XX
medx
zr
zq
ztr
dx
dmedx
dzr
dzq
dztr
nn = 10;
X = array(c(T,F),dim=c(1000)) #������ ��� ����������� ��������
MedX = array(c(T,F),dim=c(1000)) #������ ��� ���������� ������� 
ZR = array(c(T,F),dim=c(1000)) #������ ��� ��������� ������������� ���������� ���������
ZQ = array(c(T,F),dim=c(1000)) #������ ��� ��������� ���������
ZTr = array(c(T,F),dim=c(1000)) #������ ��� ���������� ��������
for(j in 1:1000){
  D <- DExp(rate = 1/sqrt(2)) #������������� �������
  selection <-r(D)(nn)            #������������� �������
  
  selection = sort(selection)
  
  #���������� �������
  s = 0
  s = sum(selection)
  X[j] = s/nn
  
  #���������� ������� (n - ������ ������)
  MedX[j] = (selection[nn / 2] + selection[nn / 2 + 1]) / 2
  
  #��������� ������������� ���������� ���������:
  a = min(selection);
  b = max(selection);
  ZR[j] = (a + b) / 2
  
  #��������� ���������
  m = nn %% 4 #mod(n, 4);
  if (m == 0){
    i = nn / 4;
  }
  if( m != 0){
    i = floor(nn / 4) + 1;
  }
  z1 = selection[i];
  z2 = selection[nn - i + 1];
  ZQ[j] = (z1 + z2)/2
  
  #��������� �������
  r = nn / 4;
  koef = 1 / (nn - 2 * r);
  s = 0;
  arr = array(c(T,F),dim=c(nn - 2*r))
  for (i in floor(r+1):ceiling(nn-r)){
    arr[i - r] = selection[i];
  }
  s = sum(arr)
  ZTr[j] = koef * s
}
XX = sum(X) / 1000  
medx = sum(MedX) / 1000
zr = sum(ZR) / 1000
zq = sum(ZQ) / 1000
ztr = sum(ZTr) / 1000


#���������� ���������

DX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DX[i] = (X[i] - XX) *  (X[i] - XX)
}
dx = sum(DX) / 1000

DMedX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DMedX[i] = (MedX[i] - medx) *  (MedX[i] - medx)
}
dmedx = sum(DMedX) / 1000


DZR = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZR[i] = (ZR[i] - zr) *  (ZR[i] - zr)
}
dzr = sum(DZR) / 1000


DZQ = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZQ[i] = (ZQ[i] - zq) *  (ZQ[i] - zq)
}
dzq = sum(DZQ) / 1000


DZTr = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZTr[i] = (ZTr[i] - ztr) *  (ZTr[i] - ztr)
}
dztr = sum(DZTr) / 1000

XX
medx
zr
zq
ztr
dx
dmedx
dzr
dzq
dztr
nn = 10;
X = array(c(T,F),dim=c(1000)) #������ ��� ����������� ��������
MedX = array(c(T,F),dim=c(1000)) #������ ��� ���������� ������� 
ZR = array(c(T,F),dim=c(1000)) #������ ��� ��������� ������������� ���������� ���������
ZQ = array(c(T,F),dim=c(1000)) #������ ��� ��������� ���������
ZTr = array(c(T,F),dim=c(1000)) #������ ��� ���������� ��������
for(j in 1:1000){
  selection <- rnorm(n = nn, mean = 0, sd  = 1);  #���������� �������������
  
  selection = sort(selection)
  
  #���������� �������
  s = 0
  s = sum(selection)
  X[j] = s/nn
  
  #���������� ������� (n - ������ ������)
  MedX[j] = (selection[nn / 2] + selection[nn / 2 + 1]) / 2
  
  #��������� ������������� ���������� ���������:
  a = min(selection);
  b = max(selection);
  ZR[j] = (a + b) / 2
  
  #��������� ���������
  m = nn %% 4 #mod(n, 4);
  if (m == 0){
    i = nn / 4;
  }
  if( m != 0){
    i = floor(nn / 4) + 1;
  }
  z1 = selection[i];
  z2 = selection[nn - i + 1];
  ZQ[j] = (z1 + z2)/2
  
  #��������� �������
  r = nn / 4;
  koef = 1 / (nn - 2 * r);
  s = 0;
  arr = array(c(T,F),dim=c(nn - 2*r))
  for (i in floor(r+1):ceiling(nn-r)){
    arr[i - r] = selection[i];
  }
  s = sum(arr)
  ZTr[j] = koef * s
}
XX = sum(X) / 1000  
medx = sum(MedX) / 1000
zr = sum(ZR) / 1000
zq = sum(ZQ) / 1000
ztr = sum(ZTr) / 1000


#���������� ���������

DX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DX[i] = (X[i] - XX) *  (X[i] - XX)
}
dx = sum(DX) / 1000

DMedX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DMedX[i] = (MedX[i] - medx) *  (MedX[i] - medx)
}
dmedx = sum(DMedX) / 1000


DZR = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZR[i] = (ZR[i] - zr) *  (ZR[i] - zr)
}
dzr = sum(DZR) / 1000


DZQ = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZQ[i] = (ZQ[i] - zq) *  (ZQ[i] - zq)
}
dzq = sum(DZQ) / 1000


DZTr = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZTr[i] = (ZTr[i] - ztr) *  (ZTr[i] - ztr)
}
dztr = sum(DZTr) / 1000

XX
medx
zr
zq
ztr
dx
dmedx
dzr
dzq
dztr
nn = 10;
X = array(c(T,F),dim=c(1000)) #������ ��� ����������� ��������
MedX = array(c(T,F),dim=c(1000)) #������ ��� ���������� ������� 
ZR = array(c(T,F),dim=c(1000)) #������ ��� ��������� ������������� ���������� ���������
ZQ = array(c(T,F),dim=c(1000)) #������ ��� ��������� ���������
ZTr = array(c(T,F),dim=c(1000)) #������ ��� ���������� ��������
for(j in 1:1000){
  selection <- rpois(n = nn, lambda = 10)  #������������� �������
  
  selection = sort(selection)
  
  #���������� �������
  s = 0
  s = sum(selection)
  X[j] = s/nn
  
  #���������� ������� (n - ������ ������)
  MedX[j] = (selection[nn / 2] + selection[nn / 2 + 1]) / 2
  
  #��������� ������������� ���������� ���������:
  a = min(selection);
  b = max(selection);
  ZR[j] = (a + b) / 2
  
  #��������� ���������
  m = nn %% 4 #mod(n, 4);
  if (m == 0){
    i = nn / 4;
  }
  if( m != 0){
    i = floor(nn / 4) + 1;
  }
  z1 = selection[i];
  z2 = selection[nn - i + 1];
  ZQ[j] = (z1 + z2)/2
  
  #��������� �������
  r = nn / 4;
  koef = 1 / (nn - 2 * r);
  s = 0;
  arr = array(c(T,F),dim=c(nn - 2*r))
  for (i in floor(r+1):ceiling(nn-r)){
    arr[i - r] = selection[i];
  }
  s = sum(arr)
  ZTr[j] = koef * s
}
XX = sum(X) / 1000  
medx = sum(MedX) / 1000
zr = sum(ZR) / 1000
zq = sum(ZQ) / 1000
ztr = sum(ZTr) / 1000


#���������� ���������

DX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DX[i] = (X[i] - XX) *  (X[i] - XX)
}
dx = sum(DX) / 1000

DMedX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DMedX[i] = (MedX[i] - medx) *  (MedX[i] - medx)
}
dmedx = sum(DMedX) / 1000


DZR = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZR[i] = (ZR[i] - zr) *  (ZR[i] - zr)
}
dzr = sum(DZR) / 1000


DZQ = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZQ[i] = (ZQ[i] - zq) *  (ZQ[i] - zq)
}
dzq = sum(DZQ) / 1000


DZTr = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZTr[i] = (ZTr[i] - ztr) *  (ZTr[i] - ztr)
}
dztr = sum(DZTr) / 1000

XX
medx
zr
zq
ztr
dx
dmedx
dzr
dzq
dztr
nn = 10;
X = array(c(T,F),dim=c(1000)) #������ ��� ����������� ��������
MedX = array(c(T,F),dim=c(1000)) #������ ��� ���������� ������� 
ZR = array(c(T,F),dim=c(1000)) #������ ��� ��������� ������������� ���������� ���������
ZQ = array(c(T,F),dim=c(1000)) #������ ��� ��������� ���������
ZTr = array(c(T,F),dim=c(1000)) #������ ��� ���������� ��������
for(j in 1:1000){
  selection <- runif(n = nn,-sqrt(3),sqrt(3))   #����������� �������������
  
  selection = sort(selection)
  
  #���������� �������
  s = 0
  s = sum(selection)
  X[j] = s/nn
  
  #���������� ������� (n - ������ ������)
  MedX[j] = (selection[nn / 2] + selection[nn / 2 + 1]) / 2
  
  #��������� ������������� ���������� ���������:
  a = min(selection);
  b = max(selection);
  ZR[j] = (a + b) / 2
  
  #��������� ���������
  m = nn %% 4 #mod(n, 4);
  if (m == 0){
    i = nn / 4;
  }
  if( m != 0){
    i = floor(nn / 4) + 1;
  }
  z1 = selection[i];
  z2 = selection[nn - i + 1];
  ZQ[j] = (z1 + z2)/2
  
  #��������� �������
  r = nn / 4;
  koef = 1 / (nn - 2 * r);
  s = 0;
  arr = array(c(T,F),dim=c(nn - 2*r))
  for (i in floor(r+1):ceiling(nn-r)){
    arr[i - r] = selection[i];
  }
  s = sum(arr)
  ZTr[j] = koef * s
}
XX = sum(X) / 1000  
medx = sum(MedX) / 1000
zr = sum(ZR) / 1000
zq = sum(ZQ) / 1000
ztr = sum(ZTr) / 1000


#���������� ���������

DX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DX[i] = (X[i] - XX) *  (X[i] - XX)
}
dx = sum(DX) / 1000

DMedX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DMedX[i] = (MedX[i] - medx) *  (MedX[i] - medx)
}
dmedx = sum(DMedX) / 1000


DZR = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZR[i] = (ZR[i] - zr) *  (ZR[i] - zr)
}
dzr = sum(DZR) / 1000


DZQ = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZQ[i] = (ZQ[i] - zq) *  (ZQ[i] - zq)
}
dzq = sum(DZQ) / 1000


DZTr = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZTr[i] = (ZTr[i] - ztr) *  (ZTr[i] - ztr)
}
dztr = sum(DZTr) / 1000

XX
medx
zr
zq
ztr
dx
dmedx
dzr
dzq
dztr
