FROM semtech/mu-cl-resources:1.17.1

COPY . /app/resources-generator/
ADD ./startup.lisp /usr/src/startup.lisp

CMD sh /load-config.sh; sbcl --load /usr/src/startup.lisp
