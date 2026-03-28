Mortalità in Italia tra 2011 e 2025
================

Lorenzo Ruffino
[@Ruffino_Lorenzo](https://twitter.com/Ruffino_Lorenzo)

## Fonti

Tutti i dati sono Istat:

- **Decessi**: [dati di mortalità giornalieri comunali](https://www.istat.it/notizia/dati-di-mortalita-cosa-produce-listat/)
- **Popolazione 2011–2018**: [Ricostruzione intercensuaria](https://demo.istat.it/app/?i=RIC&l=it)
- **Popolazione 2019–2025**: [Popolazione per età e sesso al 1° gennaio](https://demo.istat.it/app/?i=POS&l=it)

## Standardizzazione dei tassi di mortalità

Il confronto della mortalità tra anni diversi richiede di tenere conto dei cambiamenti nella struttura per età della popolazione: se la popolazione invecchia, i decessi aumentano anche a parità di rischio effettivo. Per questo si usano tassi standardizzati con il metodo della **standardizzazione diretta**.

La popolazione di riferimento è la struttura per età e genere dell'Italia al 1° gennaio 2025. Per ciascuna combinazione di fascia anagrafica e genere si calcola il tasso di mortalità osservato in quell'anno (decessi / popolazione). Ogni tasso viene poi pesato in base alla quota che quella fascia rappresenta nella popolazione di riferimento. La somma dei tassi pesati, moltiplicata per 100.000, dà il **tasso standardizzato** per 100.000 abitanti.

In formula:

$$\text{tasso std} = \sum_{i} \frac{d_i}{p_i} \cdot \frac{p_i^{\text{std}}}{P^{\text{std}}} \times 100.000$$

dove $d_i$ e $p_i$ sono decessi e popolazione nella fascia $i$ nell'anno considerato, $p_i^{\text{std}}$ è la popolazione nella fascia $i$ nel 2025 e $P^{\text{std}}$ è la popolazione totale di riferimento.

Questo metodo viene applicato a livello nazionale, per regione e per genere. Per i confronti 2025 vs media 2015–2019, il baseline è la media dei tassi standardizzati dei cinque anni 2015–2019.
