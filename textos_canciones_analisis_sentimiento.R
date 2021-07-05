library(dplyr)
library(tidytext)
library(ggplot2)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tm)
library(quanteda)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

alas <- c(
  "Que tiemble el Estado, los cielos, las calles",
  "Que tiemblen los jueces y los judiciales",
  "Hoy a las mujeres nos quitan la calma",
  "Nos sembraron miedo, nos crecieron alas",
  "A cada minuto, de cada semana",
  "Nos roban amigas, nos matan hermanas",
  "Destrozan sus cuerpos, los desaparecen",
  "No olvide sus nombres, por favor, señor presidente",
  "Por todas las compas marchando en Reforma",
  "Por todas las morras peleando en Sonora",
  "Por las comandantas luchando por Chiapas",
  "Por todas las madres buscando en Tijuana",
  "Cantamos sin miedo, pedimos justicia",
  "Gritamos por cada desaparecida",
  "Que resuene fuerte ¡nos queremos vivas!",
  "Que caiga con fuerza el feminicida",
  "Yo todo lo incendio, yo todo lo rompo",
  "Si un día algún fulano te apaga los ojos",
  "Ya nada me calla, ya todo me sobra",
  "Si tocan a una, respondemos todas",
  "Soy Claudia, soy Esther y soy Teresa",
  "Soy Ingrid, soy Fabiola y soy Valeria",
  "Soy la niña que subiste por la fuerza",
  "Soy la madre que ahora llora por sus muertas",
  "Y soy esta que te hará pagar las cuentas",
  "¡Justicia, justicia, justicia!",
  "Por todas las compas marchando en Reforma",
  "Por todas las morras peleando en Sonora",
  "Por las comandantas luchando por Chiapas",
  "Por todas las madres buscando en Tijuana",
  "Cantamos sin miedo, pedimos justicia",
  "Gritamos por cada desaparecida",
  "Que resuene fuerte ¡nos queremos vivas!",
  "Que caiga con fuerza el feminicida",
  "Que caiga con fuerza el feminicida",
  "Y retiemblen sus centros la tierra",
  "Al sororo rugir del amor",
  "Y retiemblen sus centros la tierra",
  "Al sororo rugir del amor"
)


# se hace un tibble
alas_df <- tibble(line = 1:39, texto = alas)

# tibble de dos columnas 
alas_df <- alas_df %>%
  unnest_tokens(word, texto)

#### stop_words en español
tm_stop_words <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                      lexicon = "custom"))

alas_df_0 <- alas_df %>% 
  anti_join(tm_stop_words)

alas_df_0 %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#8d99ae") +
  labs(title = "Frecuencia de palabras en la CANCION SIN MIEDO", 
       x = "FRECUENCIA", y = " ", color = "", size = 40) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "#88398A")) +
  theme(axis.title.y = element_text(size = 18, face="bold", color = "#88398A"),
        axis.title.x = element_text(size = 18, face="bold", color = "#88398A"),
        axis.text = element_text(size = 16, face="bold", color = "#88398A"))


alas_df_00 <- alas_df_0 %>%
  count(word, sort = TRUE) 

wordcloud(
  words = alas_df_00$word,
  freq = alas_df_00$n,
  min.freq =1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)


querida <- c(
  "Si observas desde afuera no lo notas",
  "Pero una duda crece en mi cabeza",
  "¿Será que lo mejor es ir a prisa",
  "Mientras doy vuelta en la esquina",
  "De los ojos que me acechan?",
  "¿Será que les aguanto la mirada?",
  "¿O será mejor andar",
  "Y dar la vuelta en otra cuadra?",
  "La otra cuadra es un poco lo mismo",
  "Ciudad exponencialmente pesada",
  "¿Será que sólo soy placer gratuito",
  "Un anaquel abastecido",
  "Mire a ver si algo le agrada?",
  "¿Será que en esta curva de cadera",
  "Mi vida corre peligro",
  "Más que en la de carretera?",
  "Mi madre me decía Ten cuidado",
  "Mejor no andar de noche por las calles",
  "Y fíjate muy bien que cualquier trago que te tomes",
  "Te lo sirvan cuando estés ahí delante",
  "A mis hermanos no se qué les dijo",
  "No se si le mortifique",
  "Que alguna mujer los mate",
  "Ay, Querida Muerte",
  "No, no vengas hoy",
  "Ay, Querida Muerte",
  "No vengas hoy",
  "No todas correremos con la suerte",
  "Estar de suerte ahora es estar viva",
  "Ahora estar de suerte es que tu novio",
  "No resulte violador, abusador o femicida",
  "Ahora estar de suerte es que a la muerte",
  "No le guste tu cintura y en su troca no te siga",
  "Yo ya no sé qué hacer con esta rabia",
  "Lo mismo mis amigas y pareja",
  "Denuncias y denuncias y denuncias y denuncias",
  "Y nomás no se ve nadie tras las rejas",
  "Están libres afuera emborrachando a alguna chica",
  "Para ver si en unas horas la cortejan",
  "Ay, Querida Muerte",
  "No, no vengas hoy",
  "Ay, Querida Muerte",
  "No vengas...",
  "No nos maten",
  "No",
  "Ya no nos maten",
  "No nos maten No",
  "Ya no nos maten",
  "Si observas desde afuera no lo notas",
  "Pero una duda crece en mi cabeza",
  "¿Será que lo mejor es ir a prisa",
  "Mientras doy vuelta en la esquina",
  "De los hombres que me acechan?",
  "¿Será que les aguanto la mirada?",
  "¿O será mejor andar",
  "Y dar la vuelta en otra cuadra?"
)

# se hace un tibble
querida_df <- tibble(line = 1:56, texto = querida)

# tibble de dos columnas 
querida_df <- querida_df %>%
  unnest_tokens(word, texto)

#### stop_words en español
tm_stop_words <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                      lexicon = "custom"))

querida_df_0 <- querida_df %>% 
  anti_join(tm_stop_words)

querida_df_0 %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#8d99ae") +
  labs(title = "Frecuencia de palabras en la QUERIDA MUERTE", 
       x = "FRECUENCIA", y = " ", color = "", size = 40) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "#88398A")) +
  theme(axis.title.y = element_text(size = 18, face="bold", color = "#88398A"),
        axis.title.x = element_text(size = 18, face="bold", color = "#88398A"),
        axis.text = element_text(size = 16, face="bold", color = "#88398A"))


querida_df_00 <- querida_df_0 %>%
  count(word, sort = TRUE) 

wordcloud(
  words = querida_df_00$word,
  freq = querida_df_00$n,
  min.freq =1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)



### antipatriota
anti <- c(
  "Yo puedo ser tu hermana tu hija, Tamara, Pamela o Valentina",
  "Yo puedo ser tu gran amiga, incluso tu compañera de vida",
  "Yo puedo ser tu gran aliada, la que aconseja y la que apaña",
  "Yo puedo ser cualquiera de todas, depende de cómo tú me apodas",
  "Pero no voy a ser la que obedece porque mi cuerpo me pertenece",
  "Yo decido de mi tiempo, cómo quiero y dónde quiero",
  "Independiente yo nací, independiente decidí",
  "Yo no camino detrás de ti, yo camino de la par a ti",
  "Tú no me vas a humillar, tú no me vas a gritar",
  "Tú no me vas someter, tú no me vas a golpear",
  "Tú no me vas denigrar, tú no me vas obligar",
  "Tú no me vas a silenciar, tú no me vas a callar",
  "No sumisa ni obediente",
  "Mujer fuerte insurgente",
  "Independiente y valiente",
  "Romper las cadenas de lo indiferente",
  "No pasiva ni oprimida",
  "Mujer linda que das vida",
  "Emancipada en autonomía", 
  "Antipatriarca y alegría",
  "A liberar, a liberar, a liberar, ah ah",
  "Libera, libera, libera",
  "Yo puedo ser jefa de hogar, empleada o intelectual",
  "Yo puedo ser protagonista de nuestra historia y la que agita",
  "La gente la comunidad, la que despierta la vecindad",
  "La que organiza la economía de su casa de su familia",
  "Mujer líder se pone de pie",
  "Y a romper las cadenas de la piel",
  "Tú no me vas a humillar, tú no me vas a gritar",
  "Tú no me vas someter, tú no me vas a golpear",
  "Tú no me vas denigrar, tú no me vas obligar",
  "Tú no me vas a silenciar, tú no me vas a callar",
  "No sumisa ni obediente",
  "Mujer fuerte insurgente",
  "Independiente y valiente",
  "Romper las cadenas de lo indiferente",
  "No pasiva ni oprimida",
  "Mujer linda que das vida",
  "Emancipada en autonomía",
  "Antipatriarca y alegría",
  "A liberar, a liberar, a liberar, ah ah",
  "Libera, libera, libera",
  "Libera, libera, libera",
  "Libera, libera, libera"
)

# se hace un tibble
anti_df <- tibble(line = 1:44, texto = anti)

# tibble de dos columnas 
anti_df <- anti_df %>%
  unnest_tokens(word, texto)

#### stop_words en español
tm_stop_words <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                      lexicon = "custom"))

anti_df_0 <- anti_df %>% 
  anti_join(tm_stop_words)

anti_df_0 %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#8d99ae") +
  labs(title = "Frecuencia de palabras en la ANTIPATRIOTA", 
       x = "FRECUENCIA", y = " ", color = "", size = 40) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "#88398A")) +
  theme(axis.title.y = element_text(size = 18, face="bold", color = "#88398A"),
        axis.title.x = element_text(size = 18, face="bold", color = "#88398A"),
        axis.text = element_text(size = 16, face="bold", color = "#88398A"))


anti_df_00 <- anti_df_0 %>%
  count(word, sort = TRUE) 

wordcloud(
  words = anti_df_00$word,
  freq = anti_df_00$n,
  min.freq =1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)




# bebe




##### brr 
bad <- c(
  "Hoy la noche se acaba",
  "Tú desnuda en mi cama",
  "Anoche te soñé",
  "Y me quedé con las gana uah",
  "Baby, apaga el celular, que te quiero disfrutar",
  "No le vayas a contestar, no, yeh",
  "Y ese traje se ve bien con tus nalga apretáda",
  "Pero es hora de quitarlo",
  "Ey, hoy la noche es tuya y mía",
  "La luz ya está apagáda, pero tú estás prendída",
  "La nota me dice que siga",
  "Te voy a dar hasta que Dios diga, ey",
  "Hoy la noche es tuya y mía",
  "La luz está apagáda, pero tú estás prendída",
  "La nota me dice que siga",
  "Te voy a dar hasta que Dios diga, uah",
  "Estás tan dura que hasta Ricky Martin quiere darte",
  "Tú llegas y estas putas empiezan a envidiarte",
  "Tu gato dice que es real, pero él te miente",
  "Y eso es real, eso es real hasta la muerte, yah",
  "¿Por qué ese jevo tuyo está celoso? Está celoso",
  "Y esos labios allá abajo tán jugosos jugosos",
  "Baby, hoy la noche es de nosotros",
  "Y ese totito en mi boca está chicloso, ey",
  "Que nos perdone La Virgen y Jesucristo",
  "Fue que yo te vi, me tropecé con tu culito",
  "Bebé, no me compares, porque yo nunca compito",
  "Mayweather no se va a trepar al ring con Margarito",
  "Yo solo quiero perrearte, en la cama amarrarte",
  "Morderte y lamberte todas tus partes",
  "Hablándote al oído groserías",
  "Tu amor es de él, pero en el sexo tú eres mía",
  "Hoy la noche es tuya y mía",
  "La luz está apagáda, pero tú estás prendída",
  "La nota me dice que siga",
  "Te voy a dar hasta que Dios diga",
  "Hoy la noche es tuya y mía",
  "La luz está apagáda, pero tú estás prendída",
  "La nota me dice que siga",
  "Te voy a dar hasta que Dios diga",
  "Ese culo está cabrón, mami, tú estás bendecida",
  "Te toqué y estabas humedecida",
  "Para chingar, no hay que estar juntos de por vida",
  "Dejemos que la Luna hoy decida",
  "Porque nosotros estamos arrebatado arrebatado",
  "Y tú bien bellaca, ya yo lo he notado yo lo he notado",
  "El gato tuyo siempre coge todo prestado",
  "Y yo a 40 mil pies en el jet trepado",
  "Tú eres mi Lady Gaga, yo tu Bradley Cooper",
  "Ella se lo traga y me lo escupe",
  "Tú quieres comerme, yo siempre lo supe",
  "Si quieres, te compro la Range, o la Mini Cooper",
  "Y te compro un babydoll Gucci",
  "Para que te lo pongas con los tacones Prada",
  "Te veo typing, pero no me envías nada",
  "Tírame el location y espérame en la entrada, ey",
  "Que te compro un babydoll Gucci",
  "Para que te lo pongas con los tacones Prada",
  "Te veo typing, pero no me envías nada",
  "Envíame el location y espérame en la entrada, ey",
  "Hoy la noche es tuya y mía",
  "La luz ya está apagáda, pero tú estás prendída",
  "La nota me dice que siga",
  "Te voy a dar hasta que Dios diga uah",
  "Hoy la noche es tuya y mía",
  "La luz está apagáda, pero tú estás prendída",
  "La nota me dice que siga",
  "Te voy a dar hasta que Dios diga, ey",
  "Brr",
  "Real, real yeh, yeh, yeh, yeh",
  "Real hasta la muerte, baby yeh, yeh, yeh, yeh",
  "Real hasta la muerte, baby Bad Bunny, baby",
  "Bad Bunny, baby, yeh-yeh",
  "Anuel, Chris Jeday",
  "Ga-Gaby Music",
  "Mera, dime Nino",
  "Yeh-yeh",
  "Dulce Como Candy",
  "Yeh-yeh real hasta la muerte, baby",
  "Los reyes y los dioses",
  "¿Oíste, bebé? Real hasta la muerte, baby",
  "Brr",
  "Mera, dime Frabian",
  "Real hasta la muerte",
  "Yeh-yeh-yeh",
  "Dile que tú eres mía",
  "La luz ya está apagada, pero tú estás prendída, prendída",
  "La nota me dice que siga",
  "Te voy a dar hasta que Dios diga",
  "Yeh-yeh-yeh"
)


# se hace un tibble
bad_df <- tibble(line = 1:90, texto = bad)

# tibble de dos columnas 
bad_df <- bad_df %>%
  unnest_tokens(word, texto)

#### stop_words en español
tm_stop_words <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                      lexicon = "custom"))

bad_df_0 <- bad_df %>% 
  anti_join(tm_stop_words)

bad_df_0 %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#Masoquismo

bad <- c( "Bla, bla, bla, bla, bla, bla",
          "Ey, yo, yo-yo, yo-yo, yo-yo",
          "Yo, (la-la-la-la-la-la-la) blow, blow (la-la-la-la-la-la-la)",
          "Diablo', qué safaera",
          "Tú tiene' un culo cabrón",
          "Cualquier cosa que te pongas rompes la carretera (la-la-la-la-la)",
          "Aight, muévelo, muévelo, muévelo, muévelo (la-la-la-la-la-la-la)",
          "Qué safaera la-la-la-la-la",
          "Tú tiene' un culo cabrón",
          "Cualquier cosa que te pongas rompes la carretera",
          "Aight, (tra) muévelo, muévelo, muévelo, muévelo",
          "Qué falta de respeto, mami",
          "¿Cómo te atreve' a venir sin panty?",
          "Hoy saliste puesta pa' mí",
          "Yo que pensaba que venía a dormir, no",
          "Vino ready ya, puesta pa' una cepillá'",
          "Me chupa la lollipop, solita se arrodilla, hey",
          "¿Cómo te atreve', mami, a venir sin panty?",
          "Mera, dímelo, DJ Orma",
          "¿Qué tú te cree'? Jodío' cabrón",
          "Yo hago lo que me da la gana, dícelo conejo",
          "Ey, ey",
          "Hoy se bebe, hoy se gasta",
          "Hoy se fuma como un rasta",
          "Si Dios lo permite (si Dios lo permite), ey",
          "Si Dios lo permite (que si Dios lo permite), ey",
          "Hoy se bebe, hoy se gasta",
          "Hoy se fuma como un rasta (wuh, wuh, wuh)",
          "Si Dios lo permite, ey",
          "Si Dios lo permite (yo, yo), ey",
          "Real G, orientando a la' generaciones nueva', con la verdadera",
          "Bellaqueo a lo galactic",
          "Sí, pa' que se te mojen los panty, métele bellaco a lo versátil",
          "Más puta que Betty Boop, la que se puso bellaca, mami, fuiste tú",
          "Sigo matando con la U",
          "Chocha con bicho, bicho con nalga (empuja)",
          "Cho-chocha con bicho, bicho con nalga, sí (empuja)",
          "Chocha con bicho, bicho con nalga (empuja)",
          "Te-te está rozando mi tetilla (empuja)",
          "Este año no quiero putilla (empuja)",
          "Te ven con mucha' prenda' y se quieren pegar (empuja)",
          "Te ven bien activao' y se quieren pegar (empuja)",
          "Porque estás bien buena, porque estás bien buena (empújamelo completo)",
          "Tetas bien grande' como Lourdes Chacón",
          "Las nalga' bien grande' como Iris Chacón",
          "La chocha no sé porque no la he visto",
          "Pero vamo' pa' la cama a clavarte en panty",
          "Hoy se bebe, hoy se gasta",
          "Hoy se fuma como un rasta",
          "Si Dios lo permite",
          "Si Dios lo permite, yeh-yeah",
          "Hoy se bebe, hoy se gasta",
          "Hoy se fuma como un rasta",
          "Si Dios lo permite",
          "Si Dios lo permite",
          "Mami ¿Qué tú quiere'? Aquí llegó tu tiburón",
          "Yo quiero perrearte y fumarme un blunt",
          "Ver lo que esconde ese pantalón",
          "Yo quiero perrearte y perrearte y perrearte (duro, duro)",
          "Yo-yo-yo-yo quiero perrearte y fumarme un blunt (duro, duro)",
          "Yo quiero perrearte y perrearte y perrear (duro, duro)",
          "Yo-yo-yo-yo quiero perrearte y fumarme un blunt, -me un blunt (duro, duro)",
          "La rola ya me explotó",
          "La nena bailando se botó",
          "Ese culo se merece to', se merece to', se merece to', yes",
          "Ese culo se merece to', se merece to', se merece to' (ey, ey, ey, ey, ey)",
          "Ah, yo pensaba que se ponía lenta",
          "'Tá bien, 'tá bien, vamo' de nuevo, de nuevo",
          "Meren a Orma, meren a Orma que está bellaco",
          "Mi bicho anda fugao' y yo quiero que tú me lo esconda'",
          "Agárralo como bonga",
          "Se metió una pepa que la pone cachonda",
          "Chinga en lo' Audi, no en lo' Honda",
          " Ey, si te lo meto no me llame'",
          "Que esto no es pa' que me ame', ey",
          "Si tu novio no te mama el culo",
          "Pa' eso que no mame",
          "Baja pa' casa que yo te lambo toa'",
          "Mami, yo te lambo toa'",
          "Baja pa' casa que yo te rompo toa', ey",
          "Que yo te rompo toa'",
          "Baja pa' casa que yo te lambo toa' (sigue)",
          "Mami, yo te lambo toa' (sigue)",
          "Dime, sierva (papi, sigue)",
          "Si tú fumas yerba (papi, pa-pa)",
          "Jowell, bebé, bebé, bebé",
          "Perreando e' la bichota (wah, oh)",
          "Se ve que chinga rico en la nota",
          "Yo quiero tirarme un selfie con esa' nalgota' (tra, tra) (oh)",
          "Parao', parao', parao' lo tengo, se me nota (woh)",
          "¿Qué vamo' a hacer con esa' nalgota'? (What)",
          " En la uni to' son A, A, A (tra)",
          "Pero esa' teta' son C",
          "Tú ere' una súper bellaca (wuh), mami, yo lo sé (eh)",
          "Yo también soy un bellaco (tra) ¿Qué vamo' a hacer? (Tú sabe') eh",
          "Con ese bum-bum, guíllate, bum-bum",
          "Guíllate ese bum-bum, guíllate, bum-bum",
          "Si tiene' ese bum-bum, guíllate, bum-bum",
          "Si tiene' ese bum-bum, guíllate, buoh"
          
)

# se hace un tibble
bad_df <- tibble(line = 1:99, texto = bad)

# tibble de dos columnas 
bad_df <- bad_df %>%
  unnest_tokens(word, texto)

#### stop_words en espaÃ±ol
tm_stop_words <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                      lexicon = "custom"))

bad_df_0 <- bad_df %>% 
  anti_join(tm_stop_words)

bad_df_0 %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#b3e2cd") +
  labs(title = "Frecuencia de palabras", 
       x = "FRECUENCIA", y = " ", color = "", size = 30) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "#88398A")) +
  theme(axis.title.y = element_text(size = 18, face="bold", color = "#88398A"),
        axis.title.x = element_text(size = 18, face="bold", color = "#88398A"),
        axis.text = element_text(size = 16, face="bold", color = "#88398A"))


bad_df_1 <- bad_df_0 %>%
  count(word, sort = TRUE) 

library(wordcloud2)

wordcloud(
  words = bad_df_1$word,
  freq = bad_df_1$n,
  min.freq =1,
  max.words=200,
  random.order=TRUE,
  rot.per=0.35,
  colors=brewer.pal(8, "Spectral")
)

wordcloud2(data=maso_df_1, size= 1)

wordcloud2(maso_df_1, size=1, color=rep_len( c("#fb8072","#bebada"), nrow(maso_df_1) ) )


#Ya fue

ya_fue <- c(
  "Ya fue, se termina",
  "No hubo gritos, ni reproches",
  "Ni portazos, ni tampoco un sin ti me muero",
  "Ya fue, y nunca volvera",
  "Y por momentos se me da la vuelta tanta rebeldia",
  "Me cuesta aprender a llorar",
  "Y ahora solo me queda cuidar de mi pena",
  "Para que no se convierta en puñales",
  "Y se quede a vivir en mi saliva",
  "Solo espero que esta vez entre la rabia y la tristeza",
  "No hagan una fiesta de disfraces dentro de mi",
  "Ya fue, se termina",
  "No hubo gritos, ni reproches",
  "Ni portazos, ni tampoco un sin ti me muero",
  "Ya fue y nunca volvera",
  "Y por momentos se me da la vuelta tanta rebeldia",
  "Me cuesta aprender a llorar",
  "Y me alegro y me animo",
  "Se que sigue mi camino",
  "Y al pasar por donde andabamos",
  "Todo vuelve a parar",
  "Y en el fondo algo me dice",
  "Que aunque el aire sea tan frio",
  "Hay dolores que me ayudan a crecer",
  "Aunque se desde hace tiempo",
  "Ando huyendo del vacio",
  "No me olvido de las cosas que encontramos",
  "Ya fue, se termina",
  "No hubo gritos, ni reproches",
  "Ni portazos, ni tampoco un sin ti me muero",
  "Ya fue, y nunca volvera",
  "Y por momentos se me da la vuelta tanta rebeldia",
  "Me cuesta aprender a poder llorar",
  "Ya fue, se termina",
  "No hubo gritos, ni reproches, ni portazos",
  "Aunque hubo algun sin ti me muero (que le vamos a hacer)",
  "Y ya fue, y nunca volvera (o eso dije la ultima vez)",
  "Y por momentos se me da la vuelta tanta rebeldia",
  "Que dificil es desaprender",
  "Voy despacio para llegar lejos",
  "Y ando lejos de donde empece",
  "Que dificil Que dificil Que dificil es",
  "Desaprender"
  
)

# se hace un tibble
ya_fue_df <- tibble(line = 1:43, texto = ya_fue)
# tibble de dos columnas 
ya_fue_df <- ya_fue_df %>%
  unnest_tokens(word, texto)

#### stop_words en espaÃ±ol
tm_stop_words <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                      lexicon = "custom"))

ya_fue_df_0 <- ya_fue_df %>% 
  anti_join(tm_stop_words)

ya_fue_df_0 %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#8d99ae") +
  labs(title = "Frecuencia de palabras de YA FUE", 
       x = "FRECUENCIA", y = " ", color = "", size = 40) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "#88398A")) +
  theme(axis.title.y = element_text(size = 18, face="bold", color = "#88398A"),
        axis.title.x = element_text(size = 18, face="bold", color = "#88398A"),
        axis.text = element_text(size = 16, face="bold", color = "#88398A"))


ya_fue_df_1 <- ya_fue_df_0 %>%
  count(word, sort = TRUE) 

wordcloud(
  words = ya_fue_df_1$word,
  freq = ya_fue_df_1$n,
  min.freq =1,
  max.words=200,
  random.order=TRUE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)

wordcloud2(data=maso_df_1, size= 1)

wordcloud2(ya_fue_df_1, size=1.5, color=rep_len( c("#fb8072","#bebada","#80b1d3"), nrow(ya_fue_df_1) ) )

########  ANALISIS DE SENTIMIENTO

X13428_2015_700_MOESM1_ESM <- read_csv("13428_2015_700_MOESM1_ESM.csv", 
                                       +     locale = locale(encoding = "WINDOWS-1252"))


diccionario<- X13428_2015_700_MOESM1_ESM

diccionario_fil <- diccionario %>% 
  filter(!between(ValenceMean,4,6.07))

bad_bunny_diccionario <- 
  bad_df %>%
  unnest_tokens(input = "texto", output = "Word") %>%
  inner_join(diccionario_fil, ., by = "Word") %>%
  mutate(Tipo = ifelse( ValenceMean > 5 , "Positiva", "Negativa")) 


#graficos

g <- bad_bunny_diccionario %>%
  count(Word, Tipo, sort = TRUE) %>%
  ungroup()


g %>%
  group_by(Tipo) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(n, Word, fill = Tipo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Tipo, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


#Nube de palabras

Negativas <- g %>% 
  filter(Tipo == "Negativa") %>% 
  select(-2)


g_2 <- bad_bunny_diccionario %>%
  count(Word, Tipo, sort = TRUE) %>%
  ungroup()

Positivas <- g %>% 
  filter(Tipo == "Positiva") %>% 
  select(-2)

wordcloud2(data=Negativas, size = 1.3, minRotation = -pi/230, maxRotation = -pi/230, 
           rotateRatio = 1, shape = "circle")

wordcloud2(data=Positivas, size = 1.3, minRotation = -pi/230, maxRotation = -pi/230, 
           rotateRatio = 1, shape = "circle")


