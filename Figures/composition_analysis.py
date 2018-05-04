import numpy as np
import pandas as pd
import scipy
import scipy.stats as stats
import matplotlib.pyplot as plt
import matplotlib.cm as cm

filebase = 'composition_analysis'

plt.rcParams.update({'font.size':8, 'lines.linewidth':1.0})

fig = plt.figure('axes',figsize=(3.33, 2.0))
ax = fig.add_subplot(111)
data = pd.read_csv('%s.csv' % filebase)

kB = 1.987e-3 # kcal/mol.K
T = 300.0 # K
df_mics = kB*T*data['df_glucose']
df_mics_ref = kB*T*0.13286765

n, bins, patches = plt.hist(df_mics, 40, histtype='bar', log=False, cumulative=False,
                            density=True, edgecolor='black', facecolor='green', alpha=0.25)

shape, loc, scale = stats.lognorm.fit(df_mics)

x = np.linspace(loc, df_mics.max(), num=200)

plt.plot(x, stats.lognorm.pdf(x, shape, loc, scale), linewidth=1.0,
         color='green', label='log-normal fit')

plt.plot([df_mics_ref], [0.0], 'o', markersize=2, color='black', clip_on=False)
ax.annotate(r'$\pi_i \propto n_i^{eff}$', xy=(df_mics_ref, 0.0), xytext=(0.075, 60), ha='center',
    arrowprops={'arrowstyle': '->'})
    
ax.set_xlim([0.07, .11])
# ax.set_xticks(kB*T*[.12, .13, .14, .15, .16, .17, .18, .19])
ax.set_xlabel(r'Root-mean-square error of $\Delta G_{solv}$ (kcal/mol)')
ax.set_ylabel('Probability density')
plt.legend(loc='upper right', frameon=False)

print("Saving PDF file...")
plt.savefig('../%s.pdf' % filebase, format='pdf', dpi=600, bbox_inches='tight')
# plt.show()
