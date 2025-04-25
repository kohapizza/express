FROM haskell:8.10.7

# 必要なパッケージをインストール
RUN apt-get update -y && apt-get install -y \
    libtinfo-dev libgmp-dev zlib1g-dev git curl && \
    rm -rf /var/lib/apt/lists/*

# Stack をインストール（既存のバージョンがあっても上書き）
RUN curl -sSL https://get.haskellstack.org/ | sh -s - -f

WORKDIR /app

# stack.yaml, package.yaml, .cabal をコピー
COPY stack.yaml package.yaml *.cabal /app/

# Hpackを実行して .cabal ファイルを生成
RUN stack exec -- hpack || true

# Hackageのパッケージ情報を更新
RUN stack update

# 依存関係のインストール
RUN stack setup
RUN stack build --only-dependencies --jobs=1

# アプリケーションのコードをコピー
COPY . /app

# lightblue をクローン
RUN git clone https://github.com/kohapizza/lightblue.git /app/lightblue

# 必要そうなものをinstall
RUN apt-get update && apt-get install -y --no-install-recommends wget build-essential libreadline-dev \ 
libncursesw5-dev libssl-dev libsqlite3-dev libgdbm-dev libbz2-dev liblzma-dev zlib1g-dev uuid-dev libffi-dev libdb-dev

# 任意バージョンのpython install
RUN wget --no-check-certificate https://www.python.org/ftp/python/3.9.5/Python-3.9.5.tgz \
&& tar -xf Python-3.9.5.tgz \
&& cd Python-3.9.5 \
&& ./configure --enable-optimizations\
&& make \
&& make install

# サイズ削減のため不要なものは削除
RUN apt-get autoremove -y

# pip のインストール
RUN curl -sS https://bootstrap.pypa.io/get-pip.py | python3.9

# kwja 依存前に torch を指定バージョンで入れる
RUN pip install torch==2.5.1 \
 && pip install kwja

RUN sed -i "1i import torch\nimport omegaconf\ntorch.serialization.add_safe_globals({'omegaconf.dictconfig.DictConfig': omegaconf.DictConfig})" \
    /usr/local/lib/python3.9/site-packages/kwja/modules/base.py

# ビルド
RUN stack build

ENV LIGHTBLUE=/app/lightblue/

# 実行コマンドを設定
CMD ["stack", "run"]