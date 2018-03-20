---
title: "Introduction au GPS"
author: "Marc-André Désautels"
date: "2018-03-20"
output:
  html_document:
    keep_md: yes
    toc: yes
  word_document:
    toc: yes
  pdf_document:
    toc: yes
---



# Qu'est-ce que le GPS?

Le système GPS a été complètement déployé en 1995 par le Ministère américain de la défense qui autorise le public à s'en servir. En utilisant un récepteur GPS, un objet qui est maintenant à la portée de toutes les bourses et qu'on peut ranger dans sa poche, on peut connaître notre position à 15-20 mètres près.

Dans la plupart des techniques de positionnement, on détermine la position par rapport à des objets dont la position est connue : c'est ce qu'on appelle faire de la triangulation. Ce peut être par rapport au soleil ou aux étoiles, des antennes dans le système Loran, des satellites dans le système GPS.

# Comment fonctionne le GPS?

Un minimum de 24 satellites bougent sur des orbites autour de la Terre à une altitude d'environ 20 000 km et émettent des signaux répétés périodiquement. Ces orbites, au nombre de 6, font un angle de 55 degrés avec le plan de l'équateur. Il y a au moins 4 satellites sur chacune. La distribution des satellites est telle qu'à tout instant sur la terre on peut capter le signal d'au moins 4 satellites.

Les signaux sont captés à l'aide d'un récepteur. Le récepteur calcule sa position sur la terre. Le principe est que le récepteur mesure les temps de parcours des signaux depuis les satellites jusqu'à lui. Étant donné que chaque signal voyage à la vitesse de la lumière, cela permet de calculer la distance entre le récepteur et chacun des satellites. La donnée de la distance $d_1$ entre un satellite $S_1$ et le récepteur permet de conclure que le récepteur se trouve sur une sphère de rayon $d_1$ centrée au satellite $S_1$. Si on connaît la distance $d_2$ entre le récepteur et un deuxième satellite $S_2$, on sait que le récepteur est aussi sur la sphère de rayon $d_2$ centrée en $S_2$. L'intersection de ces deux sphères est un cercle $C$.

A FAIRE INSERER UNE IMAGE...

Enfin, si on connaît la distance $d_3$ entre le récepteur et un troisième satellite $S_3$, alors on sait que le récepteur est sur la sphère de rayon $d_3$ centrée en $S_3$. L'intersection de cette sphère avec le cercle $C$ consiste en deux points. L'un de ces deux points se trouve toujours loin de la surface de la Terre (un avion est au maximum à 12 kilomètres d'altitude, ce qui est considéré proche) et est éliminé parce qu'irréaliste. Donc, en mesurant les temps de parcours de trois signaux depuis 3 satellites jusqu'à lui le récepteur peut calculer sa position (c'est-à-dire longitude, latitude et altitude).

En pratique les choses sont un peu plus compliquées, car les temps mesurés sont très petits et il faut donc faire des mesures très précises. Les satellites sont équipés d'horloges atomiques très coûteuses et parfaitement synchronisées alors que le récepteur a une horloge de qualité moindre. En plus des 3 inconnues qui sont les coordonnées de la position du récepteur, il y a donc une quatrième inconnue : le décalage entre l'horloge du récepteur et les horloges des satellites (lequel est le même avec tous les satellites). Le récepteur a alors besoin d'une quatrième mesure du temps de parcours du signal entre un quatrième satellite et le récepteur. Il obtient alors un système de 4 équations à 4 inconnues qui sont les trois coordonnées $x$, $y$ et $z$ donnant la position du récepteur et le décalage $T$ entre l'horloge du récepteur et celle des satellites:

\[
\begin{aligned}
\left.\begin{array}{c}
\text{4 satellites} \\
\Updownarrow \\
\text{4 équations}
\end{array}\right\}
\Longleftrightarrow
\text{4 inconnues}
\left\{\begin{array}{c}
x \\
y \\
z \\
T
\end{array}\right.
\end{aligned}
\]

Ce système admet encore deux solutions dont l'une est de nouveau éliminée parce que non réaliste. C'est le récepteur qui est chargé de résoudre ce système. Comme la solution inclut le décalage $T$ entre l'horloge du récepteur et celle des satellites, le récepteur peut alors ajuster son horloge sur celle des satellites.

# Le problème du GPS

Pour simplifier le problème du GPS, nous utiliserons les coordonnées cartésiennes $xyz$, c'est-à-dire un système de coordonnées avec la Terre centrée à l'origine, l'axe des $z$ positifs pointant vers le pôle nord, et les unités de mesure étant le rayon moyen de la Terre. 

A FAIRE INSERER UNE IMAGE...

Nous allons également supposer que n'importe quel point sur la surface de la Terre satisfait à l'équation $x^2+y^2+z^2=1$. Le temps sera mesuré en millisecondes (un millième de seconde ou alors $10^{-3}$ seconde). Nous aurons besoin des constantes suivantes pour résoudre le problème:

\[
\begin{aligned}
c &= 299792458\ ms^{-1} \qquad \text{vitesse de la lumière} \\
R_t &= 6\ 378\ 137\ m \qquad \text{rayon moyen de la Terre}
\end{aligned}
\]

À partir des deux constantes précédentes,nous allons définir une troisième constante, la vitesse de la lumière en unités de rayons de la Terre et en millième de seconde, que nous pouvons calculer de la manière suivante:

\[
\begin{aligned}
v_c &= \dfrac{c}{1000R_t}
\end{aligned}
\]

Dans ce devoir, vous vous trouvez à un endroit célèbre sur la planète Terre et votre GPS reçoit simultanément 4 signaux provenant de 4 satellites différents. Ces 4 satellites vous envoient leur position en coordonnées cartésiennes et le temps en millisecondes de l'envoi du signal. Un exemple vous est donné dans la table suivante. Les nombres sont factices pour qu'il soit plus aisé de travailler avec ceux-ci, mais ils ne sont pas complètement irréalistes.

> Les nombres se trouvant dans le tableau ci-dessous ne **sont pas** ceux que vous utiliserez pour ce laboratoire. Vous allez recevoir vos propres données qui seront distribuées par votre enseignante ou votre enseignant.


 Satellite    x    y     z         t      
-----------  ---  ----  ----  ------------
     1        2    0     2     103.538332 
     2        3    -2    3     70.815246  
     3        2    -1    -3    64.737496  
     4        3    0     -2    68.627982  


<!-- \[ -->
<!-- \begin{array}{ccc} -->
<!-- \hline -->
<!-- \text{Satellite} & \text{Position $xyz$} & \text{Temps d'envoi du signal} \\ -->
<!-- \hline -->
<!-- 1 & (1,2,0) &  19.9 \\ -->
<!-- 2 & (2,0,2) & 2.4 \\ -->
<!-- 3 & (1,1,1) & 32.6 \\ -->
<!-- 4 & (2,1,0) & 19.9 \\ -->
<!-- \hline -->
<!-- \end{array} -->
<!-- \] -->

A FAIRE INSERER UNE IMAGE...

Soit $(x,y,z)$ la position du GPS sur la Terre et $t$ le temps où les signaux arrivent au GPS. Soit $d_i$ la distance entre le récepteur GPS et le satellite $i$. Pour le satellite $1$, nous pouvons mesurer $d_1$ en calculant la distance parcourue par le signal entre le moment où il a été émis et le moment où il a été reçu. Nous avons:

\[
\begin{aligned}
d_1 &= v_c(t-103.538332)
\end{aligned}
\]

Si nous utilisons la distance euclidienne, nous avons:

\[
\begin{aligned}
d_1 &= \sqrt{(x-2)^2+(y-0)^2+(z-2)^2}
\end{aligned}
\]

En combinant ces deux équations, nous avons:

\[
\begin{aligned}
\sqrt{
(x -2)^2
+(y - 0)^2
+(z -2)^2
}
&=
v_c(t-103.538332) \\
(x -2)^2
+(y - 0)^2
+(z -2)^2
&=
v_c^2(t-103.538332)^2
\end{aligned}
\]

Si nous développons et réarrangons les termes pour que les variables linéaires soient à gauche du symbole d'égalité  (en utilisant le fait que $v_c \approx 0.047003$), nous obtenons:



\[
\begin{aligned}
4 x
+ 0 y
+4 z
-5.7376576636\times 10^{4} t
&=
-2.970329515432\times 10^{6}
+x^2+y^2+z^2-v_c^2 t^2
\end{aligned}
\]

Nous pouvons faire de même pour les trois autres satellites.

\[
\begin{aligned}
6 x
-4 y
+6 z
-3.9242822719\times 10^{4} t
&=
-1.389473076184\times 10^{6}
+x^2+y^2+z^2-v_c^2 t^2 \\
4 x
-2 y
-6 z
-3.5874790105\times 10^{4} t
&=
-1.161208047207\times 10^{6}
+x^2+y^2+z^2-v_c^2 t^2 \\
6 x
+ 0 y
-4 z
-3.8030733412\times 10^{4} t
&=
-1.304973252957\times 10^{6}
+x^2+y^2+z^2-v_c^2 t^2
\end{aligned}
\]

Remarquons que les termes quadratiques des quatre équations sont tous identiques. Nous pouvons donc les faire disparaître en faisant la différence entre les équations 2, 3 et 4 avec l'équation 1. Nous voulons donc trouver $E_2-E_1$, $E_3-E_1$ et $E_4-E_1$. Nous obtenons.

\[
\begin{aligned}
2 x
-4 y
+2 z
+0.14459 t
&=
26.604883 \\
0 x
-2 y
-10 z
+0.171445 t
&=
20.425024 \\
2 x
+ 0 y
-8 z
+0.154255 t
&=
18.278716
\end{aligned}
\]

Nous avons donc obtenu un système d'équations linéaires de trois équations et quatre inconnues. La solution de ce système (trouvée par `Maxima`) est:

\[
\begin{aligned}
x &= 0.03012t +4.802841 \\
y &= -0.026963t -4.791865 \\
z &= -0.011752t -1.084129
\end{aligned}
\]

Pour être en mesure de trouver la valeur du paramètre $t$, nous allons remplacer les équations précédentes dans la première équation ($E_1$) que nous avons trouvé précédemment. Nous avons donc:

\[
\begin{aligned}
(x-2)^2+(y-0)^2+(z-2)^2 &= v_c^2(t-103.538332)^2 \\
(0.03012t +4.802841-2)^2
+(-0.026963t -4.791865-0)^2
+(-0.011752t -1.084129-2)^2 
&= v_c^2(t-103.538332)^2 \\
\end{aligned}
\]

Nous pouvons résoudre l'équation quadratique précédente à l'aide de `Maxima`. Nous obtenons deux valeurs de $t$.

\[
\begin{aligned}
t_1 &= -249.421146957767 \\
t_2 &= 152.733718346294
\end{aligned}
\]

En remplaçant dans les équations trouvées pour $x$, $y$ et $z$, nous obtenons deux positions possibles.


     x            y             z     
-----------  ------------  -----------
 12.315401    -11.517106    -4.015282 
 0.202504     -0.673646     0.710770  

En calculant les normes des deux vecteurs, nous obtenons pour le premier 17.33307 et pour le second 1. Puisque la seconde est plus près de 1, le second vecteur correspond à la position de l'endroit où nous nous trouvons.

Pour convertir ces coordonnées cartésiennes en coordonnées géodétiques, c'est-à-dire en latitude et longitude, nous devons utiliser les formules suivantes:

\[
\begin{aligned}
r &= \sqrt{x^2+y^2+z^2} \\
\text{latitude} &= \text{Arcsin}\left(\dfrac{z}{r}\right) \\
\text{longitude} &= \text{Arctan}\left(\dfrac{y}{x}\right)
\end{aligned}
\]

Nous nous trouvons donc à la latitude $45.297583^{\circ}$ et à la longitude $-73.26875^{\circ}$. En affichant cet endroit sur une carte, nous obtenons:

<img src="skeleton_files/figure-html/ggmap-1.png" style="display: block; margin: auto;" />

Nous sommes au cégep de Saint-Jean-sur-Richelieu.

> Pour le reste du  devoir, vous devez utiliser les informations que votre enseignant vous a distribué concernant vos satellites. De plus, vous devrez utiliser le logiciel Maxima pour faire ce devoir. Écrivez vos réponses dans votre feuille Maxima en utilisant la commande `Cell` $\rightarrow$ `Insert Text Cell`.

## Préalables

1. Définir dans maxima $c$, $R_t$ et $v_c$. Pour définir un élément dans Maxima effectuer la commande `:`. Par exemple, `c:299792458` ou `E:x^2-4=0`.


## Questions

2. Trouvez l'angle (en degrés) formé par votre satellite 1, le centre de la Terre et votre satellite 2.

3. Trouvez l'azimuth (en degrés) de votre satellite 4, c'est-à-dire l'angle formé par le satellite, le centre de la Terre et le nord géographique.

4. Trouvez l'angle (en degrés) formé par votre satellite 3, le centre de la Terre et le plan de l'équateur.

> Pour répondre aux questions suivantes, nous vous invitons à faire les étapes suivantes dans MAXIMA.

- À partir de vos données, trouvez $4$ équations similaires à l'équation~(\ref{eq:equation1}) que vous noterez (E1), (E2), (E3) et (E4). Écrivez toutes les équations dans le même format, c'est-à-dire développez vos équations, comme à l'équation~(\ref{eq:equation1}).
- Vous devriez remarquer que les 4 équations ont toutes le même terme quadratique, $x^2+y^2+z^2-v_c^2t^2$. Utilisez l'équation (E1) pour éliminer le terme quadratique, créant ainsi un système d'équations linéaires de trois équations (L1), (L2) et (L3) à 4 inconnues $x$, $y$, $z$ et $t$. Vous pouvez le faire en soustrayant l'équation (E1) de chaque autre équation, c'est-à-dire (L1)=(E2)-(E1), (L2)=(E3)-(E1) et (L3)=(E4)-(E1).
- Résolvez le système d'équations linéaires composé des équations (L1), (L2) et (L3) à l'aide du logiciel Maxima. Notez que vous avez 3 équations et 4 inconnues. Choisissez la variable appropriée comme paramètre de votre ensemble solution. 
(Remarque: Il pourrait être utile d'utiliser les commandes `expand` et `float`.)
- Définir les fonctions obtenues de votre ensemble solution à l'aide de la commande `:=`. Remplacez les fonctions dans l'équation (E1) et résolvez pour la variable indépendante (le paramètre choisi précédemment). Vous devriez obtenir une équation quadratique et vous devriez obtenir deux solutions. Déterminez quelle solution est la bonne.

5. Donnez la localisation de votre GPS en coordonnées cartésiennes. Vous devez donner votre réponse en mètre en utilisant le fait que le rayon moyen de la Terre est d'environ $6\ 378\ 137$ m.

6. Donnez la distance entre votre position et le centre de la Terre.

7. Donnez l'altitude de votre position. 

8. Vous devez maintenant convertir vos coordonnées cartésiennes en coordonnées géodétiques, c'est-à-dire en latitude et longitude. Pour ce faire, utilisez les formules suivantes.

\[
\begin{aligned}
r &= \sqrt{x^2+y^2+z^2} \\
\text{latitude} &= \text{Arcsin}\left(\dfrac{z}{r}\right) \\
\text{longitude} &= \text{Arctan}\left(\dfrac{y}{x}\right)
\end{aligned}
\]

Assurez-vous de donner vos réponses en degrés et non pas en radians. (Remarque: Il pourrait être utile d'utiliser les commandes `asin` `atan2(y,x)` ainsi que la constante `%pi`.)

9. Vous devez maintenant utiliser vos coordonnées géodétiques pour localiser la position de votre GPS. Pour ce faire, utilisez le site web [Google Maps](https://www.google.ca/maps) et tapez vos coordonnées dans le champ de recherche (entrez seulement la latitude et la longitude séparées par un espace). 

10. À quel endroit, dans la liste suivante, vous trouvez-vous?

> En raison de l'approximation utilisée pour les temps des satellites, il est fort probable que vous ne vous trouverez pas exactement sur l'endroit recherché. Par contre, vous devriez vous trouvez très près.

- L'acropole d'Athènes en Grèce
- L'Alhambra de Grenade en Espagne
- Angkor au Cambodge
- Chichén Itza au Mexique
- Le Christ Rédempteur à Rio de Janeiro au Brésil
- Le Colisée à Rome en Italie
- Les Moaïs de l'île de Pâques au Chili
- La Tour Eiffel à Paris en France
- La Grande Muraille de Chine
- La Basilique Sainte-Sophie à Istanbul en Turquie
- Les temples de Kiyomizu-dera à Kyoto au Japon
- Le Kremlin à Moscou en Russie
- Le Machu Pichu au Pérou
- Le château de Neuschwanstein en Bavière en Allemagne
- Pétra en Jordanie
- La statue de la liberté à New York aux États-Unis
- Stonehenge au Royaume-Uni
- L'opéra de Sydney en Australie
- Le Taj Mahal à Agra en Inde
- La ville de Tombouctou au Mali
- La Pyramide de Khéops en Égypte
