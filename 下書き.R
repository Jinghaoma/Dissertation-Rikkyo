a = rnorm(50,0,1)
b = rnorm(50,1,1)
c = rnorm(50,2,1)

dat = c(a,b,c)

asd2 = sd(a)^2*49
bsd2 = sd(b)^2*49
csd2 = sd(c)^2*49
am = sum(asd2)
bm = sum(bsd2)
cm = sum(csd2)

wi = sum(am,bm,cm)

datsd2 = sd(dat)^2
datm = sum(datsd2)

asdd2 = (mean(a)-mean(dat))^2*50
bsdd2 = (mean(b)-mean(dat))^2*50
csdd2 = (mean(c)-mean(dat))^2*50

bw = sum(asdd2,bsdd2,csdd2)

#sd^2*(n-1)
wii = 1^2*49*3
#mea
al_me = (0*50+1*50+2*50)/150
al = (0-al_me)^2*50+(1-al_me)^2*50+(2-al_me)^2*50
al
print(wii+al)
wii/al
print(wi+bw)

print(datm*149)
