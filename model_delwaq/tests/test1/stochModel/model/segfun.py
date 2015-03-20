mm = 2117
nn = 12
# times= ['000000000',
        # '050000000',
        # '100000000',
        # '210000000',
        # '330000000',
        # '366000000']
times= ['2010/01/01-00:00:00',
        '2010/03/01-00:00:00',
        '2010/05/01-00:00:00',
        '2010/08/01-00:00:00',
        '2010/10/01-00:00:00',
        '2011/01/01-00:00:00']
# ZONE             1     2      3      4     5     6     7        
po4 = {times[0]:[0.00, 0.01 , 0.00, 0.00, 0.00, 0.00, -0.003],
       times[1]:[0.00, 0.01 , 0.00, 0.00, 0.00, 0.00, -0.003],
       times[2]:[0.00, 0.001, 0.00, 0.00, 0.00, 0.00,  0.0  ],
       times[3]:[0.00, 0.001, 0.00, 0.00, 0.00, 0.00, -0.001],
       times[4]:[0.00, 0.01 , 0.00, 0.00, 0.00, 0.00, -0.001],
       times[5]:[0.00, 0.001, 0.00, 0.00, 0.00, 0.00, -0.001]}
# ZONE             1     2     3      4     5     6     7        
nh4 = {times[0]:[0.1 , 0.00, 0.00 , 0.00, 0.00, 0.1 , 0.00],
       times[1]:[0.1 , 0.00, 0.00 , 0.00, 0.00, 0.1 , 0.00],
       times[2]:[0.0 , 0.00, 0.00 , 0.00, 0.00, 0.1 , 0.00],
       times[3]:[0.05, 0.00, 0.00 , 0.00, 0.00, 0.1 , 0.00],
       times[4]:[0.05, 0.00, 0.00 , 0.00, 0.00, 0.1 , 0.00],
       times[5]:[0.05, 0.00, 0.00 , 0.00, 0.00, 0.00, 0.00]}
# ZONE             1     2     3      4     5     6     7        
no3 = {times[0]:[0.1 , 0.2 , 0.00 , 0.1 , 0.00, 0.00, 0.2   ],
       times[1]:[0.1 , 0.2 , 0.00 , 0.1 , 0.00, 0.00, 0.05  ],
       times[2]:[0.00, 0.00, 0.00 , 0.1 , 0.00, 0.00, 0.02  ],
       times[3]:[0.00, 0.00, 0.00 , 0.1 , 0.00, 0.00, 0.1   ],
       times[4]:[0.1 , 0.2 , 0.00 , 0.1 , 0.00, 0.00, 0.1   ],
       times[5]:[0.1 , 0.00, 0.00 , 0.1 , 0.00, 0.00, 0.1   ]}

def get_wzone(i):
    if i   <= 138 : zone = 1
    elif i <= 333 : zone = 7
    elif i <= 561 : zone = 2
    elif i <= 596 : zone = 8
    elif i <= 675 : zone = 9
    elif i <= 878 : zone = 3
    elif i <= 1027: zone = 10
    elif i <= 1123: zone = 11
    elif i <= 1291: zone = 12
    elif i <= 1593: zone = 4
    elif i <= 1804: zone = 6
    elif i <= 1923: zone = 13
    else          : zone = 5
    return zone

def get_szone(i):
    if i   <= 144 : zone = 1
    elif i <= 675 : zone = 7
    elif i <= 1027: zone = 4
    elif i <= 1123: zone = 3
    elif i <= 1484: zone = 2
    elif i <= 1536: zone = 5
    elif i <= 1573: zone = 6
    elif i <= 1887: zone = 5
    else          : zone = 2
    return zone

wqallg=dict()
wqallg['PPMaxDiat'] = {
    1 : 2.5,
    2 : 2.8,
    3 : 2.8,
    4 : 0.5,
    5 : 1.0,
    6 : 1.1,
    7 : 1.5,
    8 : 2.8,
    9 : 2.8,
    10: 1.0,
    11: 2.0,
    12: 1.0,
    13: 1.0}
wqallg['PPMaxGreen'] = {
    1 : 1.0,
    2 : 1.0,
    3 : 1.0,
    4 : 1.0,
    5 : 1.0,
    6 : 1.0,
    7 : 1.0,
    8 : 1.0,
    9 : 1.0,
    10: 1.0,
    11: 1.0,
    12: 1.0,
    13: 1.0}
wqallg['Mort0Diat'] = {
    1 : 0.1,
    2 : 0.1,
    3 : 0.1,
    4 : 0.1,
    5 : 0.2,
    6 : 0.05,
    7 : 0.1,
    8 : 0.05,
    9 : 0.1,
    10: 1.0,
    11: 0.2,
    12: 0.1,
    13: 0.2}
wqallg['Mort0Green'] = {
    1 : 0.1,
    2 : 0.1,
    3 : 0.1,
    4 : 0.1,
    5 : 0.1,
    6 : 0.1,
    7 : 0.1,
    8 : 0.1,
    9 : 0.1,
    10: 0.1,
    11: 0.1,
    12: 0.1,
    13: 0.1}    

wqsetl=dict()
wqsetl['VSedDiat'] = {
    1 : 0.0,
    2 : 0.0,
    3 : 0.0,
    4 : 0.1,
    5 : 0.0,
    6 : 0.01,
    7 : 0.0,
    8 : 0.0,
    9 : 0.0,
    10: 0.1,
    11: 0.0,
    12: 0.1,
    13: 0.1}
wqsetl['VSedGreen'] = {
    1 : 0.0,
    2 : 0.0,
    3 : 0.0,
    4 : 0.0,
    5 : 0.0,
    6 : 0.0,
    7 : 0.0,
    8 : 0.0,
    9 : 0.0,
    10: 0.0,
    11: 0.0,
    12: 0.0,
    13: 0.0}
wqsetl['VSedOOC'] = {
    1 : 0.0,
    2 : 0.0,
    3 : 0.1,
    4 : 0.0,
    5 : 0.1,
    6 : 0.1,
    7 : 0.0,
    8 : 0.0,
    9 : 0.0,
    10: 0.1,
    11: 0.0,
    12: 0.1,
    13: 0.1}
wqsetl['VSedDetC'] = {
    1 : 0.0,
    2 : 0.0,
    3 : 0.0,
    4 : 0.0,
    5 : 0.0,
    6 : 0.0,
    7 : 0.0,
    8 : 0.0,
    9 : 0.0,
    10: 0.0,
    11: 0.0,
    12: 0.0,
    13: 0.0}
    
def get_po4(time, i):
    zone = get_szone(i)
    return po4[time][zone-1]
    # return 1

def get_nh4(time, i):
    zone = get_szone(i)
    return nh4[time][zone-1]

def get_no3(time, i):
    zone = get_szone(i)
    return no3[time][zone-1]    

with open('wqbenflx_po4.dat', 'w') as f:
    for t, time in enumerate(times):
        f.write(time+'\n')
        for n in range(nn):
            i = 0
            for m in range(mm):
                i += 1
                val = get_po4(time, i)
                f.write('%e\n'%val)

with open('wqbenflx_nh4.dat', 'w') as f:
    for time in times:
        f.write(time+'\n')
        for n in range(nn):
            i = 0
            for m in range(mm):
                i += 1
                val = get_nh4(time, i)
                f.write('%e\n'%val)

with open('wqbenflx_no3.dat', 'w') as f:
    for time in times:
        f.write(time+'\n')
        for n in range(nn):
            i = 0
            for m in range(mm):
                i += 1
                val = get_no3(time, i)
                f.write('%e\n'%val)                

#parameters
with open('wqallg.inc', 'w') as f:
    for param in wqallg.keys():
        f.write("PARAMETERS '%s' ALL DATA\n"%param)
        for n in range(nn):
            i = 0
            for m in range(mm):
                i += 1
                zone = get_szone(i)
                val = wqallg[param][zone]
                f.write('  %e; WQWCMAP.INP ZONE %i\n'%(val,zone))
with open('wqsetl.inc', 'w') as f:
    for param in wqsetl.keys():
        f.write("PARAMETERS '%s' ALL DATA\n"%param)
        for n in range(nn):
            i = 0
            for m in range(mm):
                i += 1
                zone = get_szone(i)
                val = wqsetl[param][zone]
                f.write('  %e; WQWCMAP.INP ZONE %i\n'%(val,zone))                
                
                