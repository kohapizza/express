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

# # Python + pip をインストール（Debianベース）
# RUN apt-get update && apt-get install -y python3 python3-pip

# # kwja をインストール
# RUN pip3 install kwja

# ビルド
RUN stack build

# 実行コマンドを設定
CMD ["stack", "run"]
