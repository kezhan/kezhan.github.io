# Crédits

## Dessins (`hd/`, et `leger/` à moitié de résolution)

1. Héros et habitants : pack « Toon Characters » de **Kenney** (https://kenney.nl/assets/toon-characters), licence **CC0 1.0** (domaine public), recoloré et complété ; sources et licence dans `outils/personnages/sources/`.
2. Compagnons, Ombres et boss : pack « Monster Builder Pack » de **Kenney** (https://kenney.nl/assets/monster-builder-pack), licence **CC0 1.0**, et formes dessinées pour le jeu dans le même style (dont les expressions des compagnons : joie, touché, fatigue). La tête de Cavalin est dessinée d'après le pack « Animal Pack Remastered » de Kenney (CC0), sans en copier de fichier.
3. Décors de combat : pack « Background Elements Remastered » de **Kenney** (https://kenney.nl/assets/background-elements-remastered), licence **CC0 1.0**, vérifiée le 05/10/2026, et formes dessinées pour le jeu ; sources et licence dans `outils/fonds/sources/`.
4. Village, effets et icônes de l'interface : dessinés pour le jeu (`outils/village/`, `outils/effets/`, `outils/icones/`).

## Sons

Mots et phrases luxembourgeois : enregistrements du **Lëtzebuerger Online Dictionnaire** (Zenter fir d'Lëtzebuerger Sprooch, https://lod.lu), ceux des mots et de quelques phrases d'exemple du dictionnaire, copiés dans `audio/lb/` pour jouer sans réseau et coupés à leur première forme (« Hond, Hënn, Honn » devient « Hond ») ; jeu de données « LOD, Linguistesch Daten » sous licence **CC0 1.0** sur https://data.public.lu/fr/datasets/letzebuerger-online-dictionnaire-lod-linguistesch-daten/ (vérifié le 05/10/2026, licence et page copiées dans `outils/contenu/sources/`). Chaque entrée garde l'adresse de son enregistrement d'origine dans `donnees/lexique_lb.json` ; fabriqué par `outils/contenu/`.

Musique et bruitages (`audio/musique/`, `audio/sons/`), tous sous licence **CC0 1.0**, licences lues sur leur page le 05/10/2026 et copiées dans `outils/sons/sources/` :
1. Village et accueil : « Home Town » de **Juhani Junkala** (JRPG Music Pack 2 Towns, https://opengameart.org/content/jrpg-pack-2-towns).
2. Combats : « Children's March Theme » de **Cleyton Kauffman** (https://opengameart.org/content/childrens-march-theme).
3. Chef de maison : « Preparing For Battle » de **Juhani Junkala** (JRPG Music Pack 5 Action, https://opengameart.org/content/jrpg-pack-5-action).
4. Fanfares : « Win Jingle » de **Fupi** (https://opengameart.org/content/win-jingle), « Cozy Puzzle Jingle & Result » de **MintoDog** (https://opengameart.org/content/cozy-puzzle-jingle-result), « Chimey UI Sounds » de **MouseBYTE** (https://opengameart.org/content/chimey-ui-sounds), « Music Jingles » de **Kenney** (https://kenney.nl/assets/music-jingles).
5. Bruitages : « Interface Sounds » et « Impact Sounds » de **Kenney** (https://kenney.nl/assets/interface-sounds, https://kenney.nl/assets/impact-sounds).
6. Cloches, étincelles, rires, bulle et souffles : faits pour le jeu (`outils/sons/scripts/synthese.py`).

## Moteur

Phaser 3.90 (https://phaser.io), licence MIT, copie dans `vendor/phaser/` avec sa licence.
