import numpy as np
import matplotlib.pyplot as plt

data = np.genfromtxt('nelder_mead.csv', delimiter=',', names=True)
plt.rcParams.update({'font.size':8})

fig, ax = plt.subplots(2, 2, sharex='col', sharey='row', gridspec_kw={'height_ratios':[1,3.5],'width_ratios':[3,1]})
ax[0,1].axis('off')

fig.subplots_adjust(hspace=0.0)
fig.set_size_inches(w=3.5,h=3.5)
fig.subplots_adjust(hspace=0.05)
fig.subplots_adjust(wspace=0.05)

for axis in ['top','bottom','left','right']:
  for i in range(2):
    for j in range(2):
      ax[i,j].spines[axis].set_linewidth(0.5)

# Uncertainty plot:
p = ax[0,0]
p.plot(data['df_no_opt'], linewidth=0.5, linestyle = 'dotted', color = 'navy')
p.plot(data['df'], linewidth=0.5, color = 'blue')
p.set_ylabel("Uncertainty",fontsize=8)
p.set_ylim([0.023,0.026])
p.yaxis.set_ticks([0.023,0.024,0.025,0.026])

# Optimization plot:
colors=['cyan','brown','green','magenta','blue','orange','purple','red','navy']
names=['p1','p2','p3','p4','p5','p6','p7','p8','p9']
p = ax[1,0]
p.set_ylim([0,0.5])
p.yaxis.set_ticks([0.0,0.1,0.2,0.3,0.4])
p.xaxis.set_ticks([0.0,100.,200.,300.,400.])
for i in range(9):
  p.plot(data[names[i]], linewidth=0.5, color = colors[i])
p.set_xlabel("Nelder-Mead Iteration")
p.set_ylabel("Prior probability")
p.legend([r'$\pi_1$', \
          r'$\pi_2$', \
          r'$\pi_3$', \
          r'$\pi_4$', \
          r'$\pi_5$', \
          r'$\pi_6$', \
          r'$\pi_7$', \
          r'$\pi_8$', \
          r'$\pi_9$'], loc='upper left', frameon=False,fontsize=8, ncol=3)

# Comparison plot:
comp = np.genfromtxt('optimization_comparison.csv', delimiter=',', names=True)
p = ax[1,1]
width=0.5
ind = np.arange(9) + 1 - width/2
p.xaxis.set_ticks(np.arange(9) + 1)
barlist = p.bar(ind,comp['Original'],width)
for i in range(9):
  barlist[i].set_color(colors[i])
p.set_xlabel("State")
p.set_title("GMBAR\nPriors",fontsize=8,y=0.8)

plt.savefig('nelder_mead.eps', format='eps', dpi=600, bbox_inches='tight')
plt.show()

