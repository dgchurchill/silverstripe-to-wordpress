module Wordpress

open System.Net.Http
open System.Net.Http.Json
open WordPressPCL
open System.Threading.Tasks

type Config = {
    Site : string
    ClientId : string
    ClientSecret : string
    Username : string
    Password : string
}

type TokenResponse = {
    access_token : string
}

let createClient (config : Config) : Task<WordPressClient> =
    task {
        use httpClient = new HttpClient()

        use tokenRequest = new FormUrlEncodedContent(
            dict [
                "client_id", config.ClientId
                "client_secret", config.ClientSecret
                "grant_type", "password"
                "username", config.Username
                "password", config.Password
            ])

        let! tokenResponseMessage = httpClient.PostAsync("https://public-api.wordpress.com/oauth2/token", tokenRequest)
        let! tokenResponse = tokenResponseMessage.Content.ReadFromJsonAsync<TokenResponse>()

        let client = new WordPressClient($"https://public-api.wordpress.com/wp/v2/sites/{config.Site}/", defaultPath="")
        client.Auth.SetJWToken(tokenResponse.access_token) // auth method defaults to Bearer, and this sets the bearer token

        return client
    }

