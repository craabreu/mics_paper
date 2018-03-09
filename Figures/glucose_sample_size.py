import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

filebase = 'glucose_sample_size'
kB = 1.987e-3 # kcal/mol.K
T = 300.0 # K

data = pd.read_csv('%s.csv' % filebase)
data['t'] = 2.0e-5*data['N']

lw = 1.0 # line with
fs = 8 # font size

fig = plt.figure('axes',figsize=(3.33, 2.0))
ax = fig.add_subplot(111)

for axis in ['top','bottom','left','right']:
    ax.spines[axis].set_linewidth(lw)

methods = ['mbar', 'mics']
colors = ['blue', 'red']
shades = ['blue', 'red']

for method, color, shade in zip(methods, colors, shades):
    f = 'f_%s' % method
    df = 'df_%s' % method
    data[f] *= kB*T
    data[df] *= kB*T
    plt.plot(data['t'], data[f], color=color, label=method.upper(), linewidth=0.5)
    plt.fill_between(data['t'], data[f] - 1.96*data[df], data[f] + 1.96*data[df],
                     alpha=0.2, facecolor=shade, edgecolor='none', interpolate=True, label='_nolegend_')
    for factor in [-1.96, 1.96]:
        plt.plot(data['t'], data[f] + factor*data[df], color=color, label='_nolegend_', linewidth=0.1)

plt.xticks(fontsize=fs)
plt.yticks(fontsize=fs)
plt.xlabel('Production time (ns)', fontsize=fs)
plt.ylabel(r'$\Delta G_{solv}$ (kcal/mol)', fontsize=fs)
plt.legend(loc='lower right', frameon=False, fontsize=fs)

print("Saving PDF file...")
plt.savefig('%s.pdf' % filebase, format='pdf', dpi=600, bbox_inches='tight')
# plt.show()
