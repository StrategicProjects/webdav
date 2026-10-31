# webdav — pacote R (CRAN)

Interface WebDAV sobre httr2. Repo: github.com/StrategicProjects/webdav · site:
https://strategicprojects.github.io/webdav/

## Estado (2026-10-31)

- **v0.3.0 aceita pelo CRAN em 2026-10-31** (auto-check OK em
  r-devel-linux-x86_64-debian-gcc e r-devel-windows-x86_64; e-mail "on its
  way to CRAN"). Deve aparecer em cran.r-project.org/package=webdav em 1–2 dias.
- Todo o trabalho da 0.3.0 está commitado em `main` (código+testes ·
  documentação · site `docs/` · metadados de release), com tag `v0.3.0` e
  GitHub release publicados. `CRAN-SUBMISSION` aponta para `67a57d3`, o commit
  que contém o código submetido.
- Autores: lista idêntica à do `../alepe` (6 pessoas, mesmos papéis e ORCIDs) —
  conferido em 2026-10-30.

### Pendências ao retomar

- Confirmar que a 0.3.0 apareceu no CRAN (página do pacote ou
  `available.packages()`) e tratar qualquer e-mail de follow-up do CRAN.
- `.DS_Store` está versionado mas no `.gitignore`: tirar do índice com
  `git rm --cached .DS_Store` quando conveniente (ficou fora dos commits da 0.3.0).
- `webdav_0.3.0.tar.gz` e `webdav.Rcheck/` sobraram na raiz (ignorados pelo git);
  podem ser apagados.
- Próxima versão: bumpar `DESCRIPTION` para `0.3.0.9000` e abrir a seção
  `# webdav (development version)` no NEWS.md.

## Convenções do pacote (adotadas na 0.3.0)

- **Política de erros**: argumento inválido → `cli_abort`; falha de rede ou do
  servidor → `cli_warn` + retorno `FALSE` (`NULL` em `webdav_list_files`).
  Status HTTP tratado explicitamente com `req_error(is_error = ~FALSE)` e
  mensagens com `resp_status_desc()`.
- O operador `%||%` é definido em `R/utils.R` (base R só o tem a partir do
  4.4.0; o pacote declara R >= 4.1).
- Upload/download são streaming (`req_body_file()` / `req_perform(path=)` com
  tempfile + rename); não carregar corpo em memória.
- Header `Destination` (COPY/MOVE) sempre passa por `httpuv::encodeURI`, igual
  à URL da requisição.

## Como validar

- Testes 100% offline: fixtures XML do PROPFIND + `httr2::local_mocked_responses`
  (`tests/testthat/`). `testthat::test_local(".")`.
- Check local: `R CMD build . && R CMD check *.tar.gz --as-cran --no-manual
  --no-examples` — **`--no-examples` porque os exemplos batem no servidor
  público webdavserver.net** (o CRAN os roda; estão protegidos por `try()` +
  `inherits()`). A etapa "CRAN incoming feasibility" trava na rede local:
  desligar com `_R_CHECK_FORCE_SUGGESTS_=false`/`_R_CHECK_CRAN_INCOMING_=false`
  se necessário.
- A vignette `env_variables.Rmd` precisa do pacote **fortunes** instalado.
- Site: `pkgdown::build_site()`; `development: mode: release` no `_pkgdown.yml`
  — o site publica **sempre** na raiz (nunca `/dev/`). Não reintroduzir `auto`.
- `README.md` vem de `README.Rmd` (`rmarkdown::render`); apagar o `README.html`
  gerado (está no `.Rbuildignore`, mas no diretório vira NOTE no check).
