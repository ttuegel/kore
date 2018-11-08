node {
    def image = docker.image('nixos/nix')
    image.inside {
        stage('Debug') {
            sh '''
                env
                id
            '''
        }
    }
}
