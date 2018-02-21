fin = open('observations_lorenz_generated.csv','r')
fout = open('data_new.txt', 'wb')
first = True
skip_first = True
t_now = 0
nsteps = 10

for line in fin:
   line.rstrip();
   if first:
      first=False
      fout.write(line)
   else:
      if (skip_first):
         skip_first = False
      else:
          data = line.split(',')
          t_obs = float(data[0])
          dt = (t_obs - t_now)/nsteps
          for istep in range(1, nsteps+1):
             time = t_now + istep * dt
             data[0] = str(time)
             new_line = ",".join(data)
             fout.write(new_line)
          t_now=t_obs
fin.close()
fout.close()
