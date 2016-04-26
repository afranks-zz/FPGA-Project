from PIL import Image
import sys
im = Image.open(sys.argv[1])
#print (im.format, im.size, im.mode)
px = im.load()
print("(", end="")
for i in range(0,im.height):
	for ii in range(0,im.width):
		if i != 0 or ii != 0:
			print(",", end="")
		print("\"", end="")
		print("{0:b}".format(px[ii,i][0]>>4).zfill(4) + "{0:b}".format(px[ii,i][1]>>4).zfill(4) + "{0:b}".format(px[ii,i][2]>>4).zfill(4), end="")  
		print("\"", end="")
print(")")
