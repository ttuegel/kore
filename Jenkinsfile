node {
    def image = docker.image('nixos/nix')
    image.inside('-u 0:0') {
        stage('Debug') {
            sh '''
                env
                id
            '''
        }
    }
}
