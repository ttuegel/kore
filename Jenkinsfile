node {
    def image = docker.image('nixos/nix')
    image.inside('-u 0:0') {
        stage('Debug') {
            sh '''
                id
                env
            '''
        }
        stage('Hello') {
            sh 'nix run -f channel:nixos-18.09 hello -c hello'
        }
        stage('Build - Java') {
            sh 'nix run -f channel:nixos-18.09 maven jdk8 -c mvn clean verify'
        }
        stage('Build - Haskell') {
            sh '''
                export LANG='en_US.UTF-8'
                export STACK_OPTS='--allow-different-user --nix --pedantic'
                nix run -f channel:nixos-18.09 git stack gnumake -c make test-kore
            '''
        }
    }
}
