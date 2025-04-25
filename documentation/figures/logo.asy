import export;
settings.outformat = "svg";
size(15cm, 15cm);

defaultpen(1.5pt);

config.drawing.gaplength = 0.04;

path roundrect (pair bl, pair tr, real r) {
    return
        arc(bl + (r,r), r, 180, 270)
        --
        arc((tr.x, bl.y) + (-r,r), r, 270, 360)
        --
        arc(tr + (-r,-r), r, 0, 90)
        --
        arc((bl.x, tr.y) + (r,-r), r, 90, 180)
        -- cycle;
}

path p = roundrect((-1,-.85), (1,.85), .15);

real a = 1.5, b = 1;

real rad = .2;
real maxang = 100;

fitpath((path)((-1, .70){(1,0)} .. {(1,0)}( .2,-.10) .. {(1,0)}(1,-.10)));
fitpath((path)((-1,-.50){(1,0)} .. {(1,0)}( .5, .75) .. {(1,0)}(1, .70)));
fitpath((path)((-1,-.70){(1,0)} .. {(1,0)}( .4, .10) .. {(1,0)}(1, .10)));
fitpath((path)((-1, .50){(1,0)} .. {(1,0)}(-.65,.60) .. {(1,0)}(1,-.70)));
fitpath((path)((-1,-.30){(1,0)} .. {(1,0)}( .4,-.70) .. {(1,0)}(1, .50)));
fitpath((path)((-1, .30){(1,0)} .. {(1,0)}( .3,-.55) .. {(1,0)}(1,-.30)));
fitpath((path)((-1, .10){(1,0)} .. {(1,0)}(-.3, .55) .. {(1,0)}(1,-.50)));
fitpath((path)((-1,-.10){(1,0)} .. {(1,0)}( .5, .35) .. {(1,0)}(1, .30)));

path[] allpaths = getdeferredpaths();

pen purple1 = rgb("453A62");
pen purple2 = rgb("5E5086");
pen purple3 = rgb("8F4E8B");

real penwidth = 8;

void highlight (
    int index,
	pen color,
	int n = 100,
	real r = 0.05,
	pair ldir1 = rotate(90)*dir(allpaths[index],0),
	pair ldir2 = rotate(90)*dir(allpaths[index],length(p)),
	pair rdir1 = rotate(-90)*dir(allpaths[index],0),
	pair rdir2 = rotate(-90)*dir(allpaths[index],length(p)),
    real t1 = 0,
    real t2 = 0
) {
    ldir1 = unit(ldir1);
    ldir2 = unit(ldir2);
    rdir1 = unit(rdir1);
    rdir2 = unit(rdir2);
    path p = subpath(allpaths[index], t1, length(allpaths[index]) - t2);
    real tstep = length(p)/n;
    guide gleft, gright;
    real loob1 = dot(dir(p, 0), ldir1);
    real roob1 = dot(dir(p, 0), rdir1);
    if (loob1 < 0) {
        gleft = point(p,0) + scale(r / sqrt(1 - loob1^2))*ldir1;
    }
    if (roob1 < 0) {
        gright = point(p,0) + scale(r / sqrt(1 - roob1^2))*rdir1;
    }
    real loob2 = dot(dir(p, length(p)), ldir2);
    real roob2 = dot(dir(p, length(p)), rdir2);
    for (real time = 0; time <= length(p); time += tstep) {
        pair dir = dir(p, time);
        pair lp = point(p, time) + rotate(90)*scale(r)*dir;
        if (
            (loob1 <= 0 || cross(lp - point(p,0), ldir1) >= 0)
            &&
            (loob2 >= 0 || cross(lp - point(p, length(p)), ldir2) <= 0)
        ) {
            gleft = gleft .. lp;
        }
        pair rp = point(p, time) + rotate(-90)*scale(r)*dir;
        if (
            (roob1 <= 0 || cross(rp - point(p,0), rdir1) <= 0)
            &&
            (roob2 >= 0 || cross(rp - point(p, length(p)), rdir2) >= 0)
        ) {
            gright = gright .. rp;
        }
    }
    if (loob2 > 0) {
        gleft = gleft .. (point(p,length(p)) + scale(r / sqrt(1 - loob2^2))*ldir2);
    }
    if (roob2 > 0) {
        gright = gright .. (point(p,length(p)) + scale(r / sqrt(1 - roob2^2))*rdir2);
    }
    filldraw((path) (point(p,0) -- gleft -- point(p, length(p)) -- reverse(gright) -- cycle), drawpen = color, fillpen = color + opacity(0.8));
}

highlight(1, color = purple1, ldir1 = (30,-1), rdir1 = (-7,-2), ldir2 = (7,6), n = 200);
highlight(2, color = purple1, ldir2 = (2,5), rdir2 = (-5,-2));
highlight(9, color = purple1, ldir1 = (-3,1), ldir2 = (-1,1), rdir1 = (5,-2), rdir2 = (5,-5));
highlight(10, color = purple1, ldir1 = (-4,5), ldir2 = (-4,-2), rdir1 = (5,-5), rdir2 = (3,-3));
highlight(15, color = purple2, ldir1 = (-2,1), rdir1 = (2,-1));
highlight(16, color = purple2, ldir2 = (-4,5), rdir2 = (4,-6), t2 = 0.13);
highlight(21, color = purple2, ldir1 = (20,1), ldir2 = (2,1), rdir1 = (-6,-2), rdir2 = (-5,-2));
highlight(22, color = purple2, ldir1 = (30,14), ldir2 = (2,1), rdir1 = (-30,-16), rdir2 = (-15,-7), n = 200);
highlight(18, color = purple3, ldir1 = (-1,2), rdir1 = (8,-12), rdir2 = (-1,-70), n = 200);
highlight(6, color = purple3, ldir1 = (-3,5), rdir1 = (4,-5));

drawdeferred();

real r2 = 0.05;

for(int i = 0; i < 8; ++i) {
    real ang = maxang - i*(2*maxang/7);
    filldraw(circle((-1-r2, .70 - i*.2), r = r2), fillpen = white);
    draw(shift((-1-r2, .70 - i*.2)) * scale(r2) * rotate(30) * polygon(3));
    filldraw(circle((1+r2,  .70 - i*.2), r = r2), fillpen = white);
    draw(shift((1+r2, .70 - i*.2)) * scale(r2) * rotate(-30) * polygon(3));
}
