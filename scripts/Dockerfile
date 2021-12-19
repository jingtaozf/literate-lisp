FROM ubuntu:22.04
RUN apt-get update -y 
RUN apt-get install -y curl git
# RUN apt-get install -y jq
# RUN apt-get install -y bzip2 make
# RUN curl -sOL `curl -s https://api.github.com/repos/roswell/roswell/releases/latest | jq -r '.assets | .[] | select(.name|test("\\\.deb$")) | .browser_download_url'`
# RUN dpkg -i roswell_*.deb
# RUN ros install sbcl
RUN apt-get install -y sbcl
RUN apt-get install -y abcl
RUN apt-get install -y ecl
RUN curl -L -O https://github.com/Clozure/ccl/releases/download/v1.12/ccl-1.12-linuxx86.tar.gz
RUN tar xfvz ccl-1.12-linuxx86.tar.gz -C /usr/local/lib/
RUN ln -sf /usr/local/lib/ccl/lx86cl64 /usr/bin/ccl
RUN curl -kLO https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --non-interactive --load quicklisp.lisp \
      --eval "(quicklisp-quickstart:install)" \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))"
# Setup lisp initialization files      
RUN cp -f ~/.sbclrc ~/.abclrc && cp -f ~/.sbclrc ~/.ecl && cp -f ~/.sbclrc ~/ccl-init.lisp 
RUN abcl --eval "(quit)"
RUN ecl --eval "(quit)"
RUN ccl --eval "(quit)"
RUN apt-get clean
