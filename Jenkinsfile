pipeline {
    agent { docker { image 'maven:3' } }
    stages {
        stage('Build - Java') {
            steps {
                sh 'mvn --version'
            }
        }
    }
}