size(x = 7.3 inches);
settings.outformat = 'pdf';
import export;
// import smoothmanifold;
//|return|[../main.tex]

// smpar(help = true, gridnumber = 10);
smpar(arrowmargin = .3, gaplength = .4);

real r = .5;
pen[] colors  = {red, green, blue, yellow};
pair distance  = (1.7, 0);
real op = .7;
pair ctr;
pair halfdash = (0,r*1.5);

smooth[][] sources;
int[] sizes = i(4,7,5,4);
pair shift0 = (0,0);
ctr = shift0 - distance;

for (int i = 0; i < sizes.length; ++i)
{
    smooth[] cursource = new smooth [sizes[i]];
    for (int j = 0; j < cursource.length; ++j)
    {
        cursource[j] = smooth(contour = scale(r)*ucircle, shift = shift0);
        shift0 += distance;
    }
    shift0 += distance;
    sources.push(cursource);
}

for (int i = 0; i < sources.length; ++i)
{ draw(sources[i], smoothfill = colors[i]+opacity(op)); }

for (int i = 0; i < sources.length-1; ++i)
{
    ctr += distance * (sources[i].length + 1);
    draw((ctr + halfdash)--(ctr - halfdash), dashed);
}

smooth[][] selections;
int[] sizes2 = i(2,4,3,1);
pair shift1 = (9,-8);
ctr = shift1 - distance;

for (int i = 0; i < sizes2.length; ++i)
{
    smooth[] curselection = new smooth [sizes2[i]];
    for (int j = 0; j < curselection.length; ++j)
    {
        curselection[j] = smooth(contour = scale(r)*ucircle, shift = shift1);
        shift1 += distance;
    }
    shift1 += distance;
    selections.push(curselection);
}

for (int i = 0; i < selections.length; ++i)
{ draw(selections[i], smoothfill = colors[i]+opacity(op)); }

for (int i = 0; i < selections.length-1; ++i)
{
    ctr += distance * (selections[i].length + 1);
    draw((ctr + halfdash)--(ctr - halfdash), dashed);
}

drawarrow(selections[0][0], sources[0][2], beginarrow = true, endarrow = false, points = p((6.4,-5.4)));
drawarrow(selections[0][1], sources[0][0], beginarrow = true, endarrow = false, points = p((8.5,-4.8),(2.8,-3.2)));

drawarrow(selections[1][0], sources[1][3], beginarrow = true, endarrow = false, points = p((12.5,-3.2)));
drawarrow(selections[1][1], sources[1][1], beginarrow = true, endarrow = false, points = p((15.3,-6.6)));
drawarrow(selections[1][2], sources[1][6], beginarrow = true, endarrow = false, points = p((17.5,-5.6)));
drawarrow(selections[1][3], sources[1][4], beginarrow = true, endarrow = false, points = p((18,-4.8),(16.2,-4.1)));

drawarrow(selections[2][0], sources[2][4], beginarrow = true, endarrow = false, points = p((23,-4.8),(27.8,-3.2)));
drawarrow(selections[2][1], sources[2][1], beginarrow = true, endarrow = false, points = p((23.4,-3.2)));
drawarrow(selections[2][2], sources[2][2], beginarrow = true, endarrow = false, points = p((27,-4.8)));

drawarrow(selections[3][0], sources[3][2], beginarrow = true, endarrow = false, points = p((34.2,-4.8)));

smooth[] allselected = concat(selections);
// smooth[] allselected = concat(selections[0], selections[1]);

// --------------------------

smooth[] merged = new smooth [sum(sizes2)];
pair shift2 = (12, -15);

pen getcolor (int i)
{
    if (i < 2) return red;
    if (i < 6) return green;
    if (i < 9) return blue;
    if (i < 10) return yellow;
    return nullpen;
}

for (int i = 0; i < merged.length; ++i)
{
    merged[i] = smooth(contour = scale(r)*ucircle, shift = shift2);
    shift2 += distance;
}

int[] refs = i(2,0,6,1,3,9,4,7,5,8);
for (int i = 0; i < merged.length; ++i)
{ draw(merged[i], smoothfill = getcolor(refs[i])+opacity(op)); }

pair[][] points = {
     p((11.8, -10.2))
    ,p((12.5, -12.0))
    ,p((16.2, -11.9), (21.6, -10.2))
    ,p((16.5, -11.9), (12.6, -10.2))
    ,p((17.5, -10.2))
    ,p((21.6, -11.9), (28.8, -10.2))
    ,p((21.6, -11.5), (19.8, -10.2))
    ,p((23.6, -11.9))
    ,p((22.6, -10.2))
    ,p((27.0, -10.2))
};

for (int i = 0; i < points.length; ++i)
{ drawarrow(merged[i], allselected[refs[i]], points = points[i], endarrow = false, beginarrow = true); }

// --------------------------

smooth[] shuffled = new smooth [merged.length];
int[] refs2 = i(2,5,0,3,1,4,7,9,6,8);
pair shift3 = (12, -24);

for (int i = 0; i < shuffled.length; ++i)
{
    shuffled[i] = smooth(contour = scale(r)*ucircle, shift = shift3);
    shift3 += distance;
}

for (int i = 0; i < shuffled.length; ++i)
{ draw(shuffled[i], smoothfill = getcolor(refs[refs2[i]])+opacity(op)); }

pair[][] points2 = {
     p((13.5, -18.4))
    ,p((14.7, -19.0), (20.0, -15.7))
    ,p((15.2, -19.9), (11.9, -15.8))
    ,p((17.3, -16.0))
    ,p((18.5, -19.6), (13.5, -15.8))
    ,p((19.7, -19.0))
    ,p((23.7, -19.0))
    ,p((24.9, -18.5))
    ,p((24.9, -19.2), (22.1, -15.7))
    ,p((25.9, -18.5))
};

for (int i = 0; i < points2.length; ++i)
{
    drawarrow(shuffled[i], merged[refs2[i]], points = points2[i], beginarrow = true, endarrow = false);
}

// --------------------------

shift0 -= distance;
shift0 -= (0,r);
draw(shift0--(shift0+(0,shift2.y-shift0.y+r)), dashed, L = Label("$\mathcal{M}\mathcal{C}(\mathfrak{L}, k_1)$", align = 2*E), bar = Bars());

shift2 -= (0,r);
shift2 = (shift0.x, shift2.y);
draw(shift2--(shift2+(0,shift3.y-shift2.y+r)), dashed, L = Label("$\mathcal{S}(\alpha, k_2)$", align = 2*E), bar = Bars());

export(margin = 1cm);
