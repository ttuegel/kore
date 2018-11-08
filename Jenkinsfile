node {
    def image = docker.image('nixos/nix')
    image.inside('-u 0:0') {
        stage('Debug') {
            sh '''
                env
                id
            '''
        }
        stage('Hello') {
            sh 'nix run -f channel:nixos-18.09 hello -c hello'
        }
        stage('Build - Java') {
            sh 'nix run -f channel:nixos-18.09 maven jdk8 -c mvn clean verify'
        }
        stage('Build - Haskell') {
            sh 'nix run -f channel:nixos-18.09 stack -c stack --nix --allow-different-user build --pedantic'
        }
    }
}
