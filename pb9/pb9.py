


for c in range(1,1000):
    a=0
    b=0
    while (a<=999):
        a += 1
        b  = 1000-c-a
#          while (a**2+b**2<c**2): # and b <= 1000):
#              b += 1
        if (a**2+b**2==c**2):
            print (a,b,c)
            print (a*b*c)
            break
print ("not found!")


